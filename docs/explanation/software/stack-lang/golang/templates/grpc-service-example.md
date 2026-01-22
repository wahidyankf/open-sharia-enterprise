# gRPC Service Example

Complete gRPC service implementation with protocol buffers, interceptors, and testing.

## Overview

This example demonstrates building a production-ready gRPC service with:

- Protocol buffer definitions (proto3)
- gRPC server implementation
- Unary and streaming RPCs
- Interceptors (logging, recovery, authentication)
- Error handling with status codes
- Testing with grpc_testing

## Protocol Buffer Definition

```protobuf
// api/proto/v1/user_service.proto
syntax = "proto3";

package api.v1;

option go_package = "example.com/my-go-project/api/proto/v1";

import "google/protobuf/timestamp.proto";
import "google/protobuf/empty.proto";

// UserService manages user operations
service UserService {
  // GetUser retrieves a single user by ID
  rpc GetUser(GetUserRequest) returns (GetUserResponse);

  // ListUsers retrieves multiple users with pagination
  rpc ListUsers(ListUsersRequest) returns (ListUsersResponse);

  // CreateUser creates a new user
  rpc CreateUser(CreateUserRequest) returns (CreateUserResponse);

  // UpdateUser updates an existing user
  rpc UpdateUser(UpdateUserRequest) returns (UpdateUserResponse);

  // DeleteUser deletes a user
  rpc DeleteUser(DeleteUserRequest) returns (google.protobuf.Empty);

  // StreamUsers streams users as they are created (server streaming)
  rpc StreamUsers(StreamUsersRequest) returns (stream User);

  // UploadUserDocuments uploads multiple documents (client streaming)
  rpc UploadUserDocuments(stream UploadDocumentRequest) returns (UploadDocumentsResponse);

  // ChatSupport provides bidirectional streaming for support chat
  rpc ChatSupport(stream ChatMessage) returns (stream ChatMessage);
}

// Messages

message User {
  string id = 1;
  string email = 2;
  string name = 3;
  google.protobuf.Timestamp created_at = 4;
  google.protobuf.Timestamp updated_at = 5;
}

message GetUserRequest {
  string id = 1;
}

message GetUserResponse {
  User user = 1;
}

message ListUsersRequest {
  int32 page_size = 1;
  string page_token = 2;
}

message ListUsersResponse {
  repeated User users = 1;
  string next_page_token = 2;
}

message CreateUserRequest {
  string email = 1;
  string name = 2;
}

message CreateUserResponse {
  User user = 1;
}

message UpdateUserRequest {
  string id = 1;
  string name = 2;
}

message UpdateUserResponse {
  User user = 1;
}

message DeleteUserRequest {
  string id = 1;
}

message StreamUsersRequest {
  // Empty or filter criteria
}

message UploadDocumentRequest {
  string user_id = 1;
  string filename = 2;
  bytes chunk = 3;
}

message UploadDocumentsResponse {
  int32 files_uploaded = 1;
  int64 total_bytes = 2;
}

message ChatMessage {
  string user_id = 1;
  string message = 2;
  google.protobuf.Timestamp timestamp = 3;
}
```

## Generate Go Code

```bash
# Install protoc compiler and Go plugins
go install google.golang.org/protobuf/cmd/protoc-gen-go@latest
go install google.golang.org/grpc/cmd/protoc-gen-go-grpc@latest

# Generate Go code from proto files
protoc --go_out=. --go_opt=paths=source_relative \
    --go-grpc_out=. --go-grpc_opt=paths=source_relative \
    api/proto/v1/user_service.proto
```

## gRPC Server Implementation

### Server Structure

```go
// internal/infrastructure/grpc/server.go
package grpc

import (
    "context"
    "fmt"
    "log/slog"
    "net"
    "time"

    "google.golang.org/grpc"
    "google.golang.org/grpc/keepalive"

    pb "example.com/my-go-project/api/proto/v1"
    "example.com/my-go-project/internal/config"
)

// Server represents the gRPC server
type Server struct {
    grpcServer *grpc.Server
    listener   net.Listener
    logger     *slog.Logger
}

// NewServer creates a new gRPC server
func NewServer(cfg *config.Config, logger *slog.Logger) (*Server, error) {
    // Create listener
    lis, err := net.Listen("tcp", fmt.Sprintf(":%d", cfg.GRPC.Port))
    if err != nil {
        return nil, fmt.Errorf("failed to listen: %w", err)
    }

    // Create gRPC server with options
    opts := []grpc.ServerOption{
        grpc.ChainUnaryInterceptor(
            LoggingInterceptor(logger),
            RecoveryInterceptor(logger),
        ),
        grpc.ChainStreamInterceptor(
            StreamLoggingInterceptor(logger),
            StreamRecoveryInterceptor(logger),
        ),
        grpc.KeepaliveParams(keepalive.ServerParameters{
            MaxConnectionIdle: 5 * time.Minute,
            Time:              2 * time.Hour,
            Timeout:           20 * time.Second,
        }),
        grpc.KeepaliveEnforcementPolicy(keepalive.EnforcementPolicy{
            MinTime:             5 * time.Minute,
            PermitWithoutStream: true,
        }),
    }

    grpcServer := grpc.NewServer(opts...)

    // Register services
    userService := NewUserService(logger)
    pb.RegisterUserServiceServer(grpcServer, userService)

    return &Server{
        grpcServer: grpcServer,
        listener:   lis,
        logger:     logger,
    }, nil
}

// Start starts the gRPC server
func (s *Server) Start() error {
    s.logger.Info("starting gRPC server", "addr", s.listener.Addr())

    if err := s.grpcServer.Serve(s.listener); err != nil {
        return fmt.Errorf("grpc server failed: %w", err)
    }

    return nil
}

// Stop gracefully stops the gRPC server
func (s *Server) Stop() {
    s.logger.Info("stopping gRPC server")
    s.grpcServer.GracefulStop()
    s.logger.Info("gRPC server stopped")
}
```

### Service Implementation

```go
// internal/infrastructure/grpc/user_service.go
package grpc

import (
    "context"
    "fmt"
    "io"
    "log/slog"

    "google.golang.org/grpc/codes"
    "google.golang.org/grpc/status"
    "google.golang.org/protobuf/types/known/emptypb"
    "google.golang.org/protobuf/types/known/timestamppb"

    pb "example.com/my-go-project/api/proto/v1"
    "example.com/my-go-project/internal/application"
    "example.com/my-go-project/internal/domain"
)

// UserService implements the gRPC UserService interface
type UserService struct {
    pb.UnimplementedUserServiceServer
    service *application.UserService
    logger  *slog.Logger
}

// NewUserService creates a new UserService
func NewUserService(logger *slog.Logger) *UserService {
    return &UserService{
        logger: logger,
    }
}

// GetUser retrieves a user by ID (unary RPC)
func (s *UserService) GetUser(ctx context.Context, req *pb.GetUserRequest) (*pb.GetUserResponse, error) {
    s.logger.InfoContext(ctx, "getting user", "id", req.Id)

    // Validate request
    if req.Id == "" {
        return nil, status.Error(codes.InvalidArgument, "user ID is required")
    }

    // Call application service
    user, err := s.service.GetByID(ctx, req.Id)
    if err != nil {
        if errors.Is(err, domain.ErrUserNotFound) {
            return nil, status.Error(codes.NotFound, "user not found")
        }
        s.logger.ErrorContext(ctx, "failed to get user", "error", err)
        return nil, status.Error(codes.Internal, "failed to get user")
    }

    // Convert domain model to protobuf
    return &pb.GetUserResponse{
        User: toPbUser(user),
    }, nil
}

// ListUsers retrieves multiple users with pagination (unary RPC)
func (s *UserService) ListUsers(ctx context.Context, req *pb.ListUsersRequest) (*pb.ListUsersResponse, error) {
    s.logger.InfoContext(ctx, "listing users",
        "page_size", req.PageSize,
        "page_token", req.PageToken,
    )

    // Default page size
    pageSize := req.PageSize
    if pageSize == 0 {
        pageSize = 10
    }
    if pageSize > 100 {
        return nil, status.Error(codes.InvalidArgument, "page size too large (max 100)")
    }

    users, nextToken, err := s.service.List(ctx, int(pageSize), req.PageToken)
    if err != nil {
        s.logger.ErrorContext(ctx, "failed to list users", "error", err)
        return nil, status.Error(codes.Internal, "failed to list users")
    }

    // Convert to protobuf
    pbUsers := make([]*pb.User, len(users))
    for i, u := range users {
        pbUsers[i] = toPbUser(u)
    }

    return &pb.ListUsersResponse{
        Users:         pbUsers,
        NextPageToken: nextToken,
    }, nil
}

// CreateUser creates a new user (unary RPC)
func (s *UserService) CreateUser(ctx context.Context, req *pb.CreateUserRequest) (*pb.CreateUserResponse, error) {
    s.logger.InfoContext(ctx, "creating user", "email", req.Email)

    // Validate request
    if req.Email == "" {
        return nil, status.Error(codes.InvalidArgument, "email is required")
    }
    if req.Name == "" {
        return nil, status.Error(codes.InvalidArgument, "name is required")
    }

    user, err := s.service.Create(ctx, req.Email, req.Name)
    if err != nil {
        if errors.Is(err, domain.ErrUserExists) {
            return nil, status.Error(codes.AlreadyExists, "user already exists")
        }
        s.logger.ErrorContext(ctx, "failed to create user", "error", err)
        return nil, status.Error(codes.Internal, "failed to create user")
    }

    return &pb.CreateUserResponse{
        User: toPbUser(user),
    }, nil
}

// UpdateUser updates an existing user (unary RPC)
func (s *UserService) UpdateUser(ctx context.Context, req *pb.UpdateUserRequest) (*pb.UpdateUserResponse, error) {
    s.logger.InfoContext(ctx, "updating user", "id", req.Id)

    if req.Id == "" {
        return nil, status.Error(codes.InvalidArgument, "user ID is required")
    }

    user, err := s.service.Update(ctx, req.Id, req.Name)
    if err != nil {
        if errors.Is(err, domain.ErrUserNotFound) {
            return nil, status.Error(codes.NotFound, "user not found")
        }
        s.logger.ErrorContext(ctx, "failed to update user", "error", err)
        return nil, status.Error(codes.Internal, "failed to update user")
    }

    return &pb.UpdateUserResponse{
        User: toPbUser(user),
    }, nil
}

// DeleteUser deletes a user (unary RPC)
func (s *UserService) DeleteUser(ctx context.Context, req *pb.DeleteUserRequest) (*emptypb.Empty, error) {
    s.logger.InfoContext(ctx, "deleting user", "id", req.Id)

    if req.Id == "" {
        return nil, status.Error(codes.InvalidArgument, "user ID is required")
    }

    if err := s.service.Delete(ctx, req.Id); err != nil {
        if errors.Is(err, domain.ErrUserNotFound) {
            return nil, status.Error(codes.NotFound, "user not found")
        }
        s.logger.ErrorContext(ctx, "failed to delete user", "error", err)
        return nil, status.Error(codes.Internal, "failed to delete user")
    }

    return &emptypb.Empty{}, nil
}

// StreamUsers streams users as they are created (server streaming)
func (s *UserService) StreamUsers(req *pb.StreamUsersRequest, stream pb.UserService_StreamUsersServer) error {
    ctx := stream.Context()

    s.logger.InfoContext(ctx, "streaming users")

    // Subscribe to user creation events
    events, unsubscribe := s.service.SubscribeToUserEvents(ctx)
    defer unsubscribe()

    for {
        select {
        case <-ctx.Done():
            return ctx.Err()
        case event := <-events:
            user := event.User

            if err := stream.Send(toPbUser(user)); err != nil {
                s.logger.ErrorContext(ctx, "failed to send user", "error", err)
                return status.Error(codes.Internal, "stream error")
            }
        }
    }
}

// UploadUserDocuments uploads multiple documents (client streaming)
func (s *UserService) UploadUserDocuments(stream pb.UserService_UploadUserDocumentsServer) error {
    ctx := stream.Context()

    s.logger.InfoContext(ctx, "uploading user documents")

    var totalBytes int64
    var filesUploaded int32
    currentFile := ""
    var fileData []byte

    for {
        req, err := stream.Recv()
        if err == io.EOF {
            // Client finished sending, save last file
            if currentFile != "" {
                if err := s.service.SaveDocument(ctx, req.UserId, currentFile, fileData); err != nil {
                    return status.Error(codes.Internal, "failed to save document")
                }
                filesUploaded++
            }

            // Send response
            return stream.SendAndClose(&pb.UploadDocumentsResponse{
                FilesUploaded: filesUploaded,
                TotalBytes:    totalBytes,
            })
        }
        if err != nil {
            s.logger.ErrorContext(ctx, "stream recv error", "error", err)
            return status.Error(codes.Internal, "stream error")
        }

        // New file started
        if req.Filename != currentFile && currentFile != "" {
            // Save previous file
            if err := s.service.SaveDocument(ctx, req.UserId, currentFile, fileData); err != nil {
                return status.Error(codes.Internal, "failed to save document")
            }
            filesUploaded++
            fileData = nil
        }

        currentFile = req.Filename
        fileData = append(fileData, req.Chunk...)
        totalBytes += int64(len(req.Chunk))
    }
}

// ChatSupport provides bidirectional streaming for support chat
func (s *UserService) ChatSupport(stream pb.UserService_ChatSupportServer) error {
    ctx := stream.Context()

    s.logger.InfoContext(ctx, "starting chat support")

    // Create channels for bidirectional communication
    errCh := make(chan error, 1)

    // Goroutine to receive messages from client
    go func() {
        for {
            msg, err := stream.Recv()
            if err == io.EOF {
                return
            }
            if err != nil {
                errCh <- err
                return
            }

            s.logger.InfoContext(ctx, "received chat message",
                "user_id", msg.UserId,
                "message", msg.Message,
            )

            // Process message and generate response
            response := s.generateChatResponse(ctx, msg)

            // Send response back to client
            if err := stream.Send(response); err != nil {
                errCh <- err
                return
            }
        }
    }()

    // Wait for error or context cancellation
    select {
    case err := <-errCh:
        return err
    case <-ctx.Done():
        return ctx.Err()
    }
}

// Helper functions

func toPbUser(user domain.User) *pb.User {
    return &pb.User{
        Id:        user.ID,
        Email:     user.Email,
        Name:      user.Name,
        CreatedAt: timestamppb.New(user.CreatedAt),
        UpdatedAt: timestamppb.New(user.UpdatedAt),
    }
}

func (s *UserService) generateChatResponse(ctx context.Context, msg *pb.ChatMessage) *pb.ChatMessage {
    // Simple echo response (in reality, this would call AI service)
    return &pb.ChatMessage{
        UserId:    "support-bot",
        Message:   fmt.Sprintf("You said: %s", msg.Message),
        Timestamp: timestamppb.Now(),
    }
}
```

### Interceptors

```go
// internal/infrastructure/grpc/interceptors.go
package grpc

import (
    "context"
    "log/slog"
    "runtime/debug"
    "time"

    "google.golang.org/grpc"
    "google.golang.org/grpc/codes"
    "google.golang.org/grpc/status"
)

// LoggingInterceptor logs unary RPC calls
func LoggingInterceptor(logger *slog.Logger) grpc.UnaryServerInterceptor {
    return func(ctx context.Context, req interface{}, info *grpc.UnaryServerInfo, handler grpc.UnaryHandler) (interface{}, error) {
        start := time.Now()

        resp, err := handler(ctx, req)

        duration := time.Since(start)
        code := status.Code(err)

        logger.InfoContext(ctx, "grpc unary call",
            "method", info.FullMethod,
            "duration_ms", duration.Milliseconds(),
            "status", code.String(),
        )

        return resp, err
    }
}

// RecoveryInterceptor recovers from panics in unary RPCs
func RecoveryInterceptor(logger *slog.Logger) grpc.UnaryServerInterceptor {
    return func(ctx context.Context, req interface{}, info *grpc.UnaryServerInfo, handler grpc.UnaryHandler) (resp interface{}, err error) {
        defer func() {
            if r := recover(); r != nil {
                logger.ErrorContext(ctx, "panic recovered",
                    "method", info.FullMethod,
                    "panic", r,
                    "stack", string(debug.Stack()),
                )
                err = status.Error(codes.Internal, "internal server error")
            }
        }()

        return handler(ctx, req)
    }
}

// StreamLoggingInterceptor logs streaming RPC calls
func StreamLoggingInterceptor(logger *slog.Logger) grpc.StreamServerInterceptor {
    return func(srv interface{}, ss grpc.ServerStream, info *grpc.StreamServerInfo, handler grpc.StreamHandler) error {
        start := time.Now()

        err := handler(srv, ss)

        duration := time.Since(start)
        code := status.Code(err)

        logger.InfoContext(ss.Context(), "grpc stream call",
            "method", info.FullMethod,
            "duration_ms", duration.Milliseconds(),
            "status", code.String(),
        )

        return err
    }
}

// StreamRecoveryInterceptor recovers from panics in streaming RPCs
func StreamRecoveryInterceptor(logger *slog.Logger) grpc.StreamServerInterceptor {
    return func(srv interface{}, ss grpc.ServerStream, info *grpc.StreamServerInfo, handler grpc.StreamHandler) (err error) {
        defer func() {
            if r := recover(); r != nil {
                logger.ErrorContext(ss.Context(), "panic recovered",
                    "method", info.FullMethod,
                    "panic", r,
                    "stack", string(debug.Stack()),
                )
                err = status.Error(codes.Internal, "internal server error")
            }
        }()

        return handler(srv, ss)
    }
}
```

### Testing

```go
// internal/infrastructure/grpc/user_service_test.go
package grpc

import (
    "context"
    "testing"

    "github.com/stretchr/testify/assert"
    "github.com/stretchr/testify/mock"
    "github.com/stretchr/testify/require"
    "google.golang.org/grpc/codes"
    "google.golang.org/grpc/status"

    pb "example.com/my-go-project/api/proto/v1"
)

func TestUserService_GetUser(t *testing.T) {
    tests := []struct {
        name          string
        req           *pb.GetUserRequest
        setupMock     func(*MockUserService)
        expectedError codes.Code
    }{
        {
            name: "user found",
            req:  &pb.GetUserRequest{Id: "123"},
            setupMock: func(m *MockUserService) {
                m.On("GetByID", mock.Anything, "123").Return(
                    domain.User{ID: "123", Email: "test@example.com", Name: "Test User"},
                    nil,
                )
            },
            expectedError: codes.OK,
        },
        {
            name: "user not found",
            req:  &pb.GetUserRequest{Id: "999"},
            setupMock: func(m *MockUserService) {
                m.On("GetByID", mock.Anything, "999").Return(
                    domain.User{},
                    domain.ErrUserNotFound,
                )
            },
            expectedError: codes.NotFound,
        },
        {
            name:          "empty ID",
            req:           &pb.GetUserRequest{Id: ""},
            setupMock:     func(m *MockUserService) {},
            expectedError: codes.InvalidArgument,
        },
    }

    for _, tt := range tests {
        t.Run(tt.name, func(t *testing.T) {
            // Setup
            mockService := new(MockUserService)
            tt.setupMock(mockService)

            svc := &UserService{
                service: mockService,
                logger:  slog.Default(),
            }

            // Execute
            resp, err := svc.GetUser(context.Background(), tt.req)

            // Assert
            if tt.expectedError != codes.OK {
                require.Error(t, err)
                st, ok := status.FromError(err)
                require.True(t, ok)
                assert.Equal(t, tt.expectedError, st.Code())
            } else {
                require.NoError(t, err)
                assert.NotNil(t, resp)
                assert.NotNil(t, resp.User)
            }

            mockService.AssertExpectations(t)
        })
    }
}
```

## Best Practices

1. **Use Proto3**: Modern protobuf version with simpler syntax
2. **Interceptors for Cross-Cutting Concerns**: Logging, recovery, authentication
3. **Proper Error Handling**: Use grpc/status package for error codes
4. **Graceful Shutdown**: Always call GracefulStop()
5. **Context Propagation**: Pass context through all layers
6. **Streaming Best Practices**: Handle EOF, use io.EOF for client stream completion
7. **Testing**: Use mocks for service dependencies

## Related Documentation

- [Go Web Services](../ex-so-stla-go__web-services.md) - Complete gRPC guide
- [Go Best Practices](../ex-so-stla-go__best-practices.md) - gRPC best practices
- [Go Concurrency](../ex-so-stla-go__concurrency-and-parallelism.md) - Streaming patterns

---

**Last Updated**: 2026-01-22
**Go Version**: 1.25.6
