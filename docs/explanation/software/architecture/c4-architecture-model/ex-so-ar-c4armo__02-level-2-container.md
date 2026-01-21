# Level 2: Container Diagram

## Overview

**Purpose**: Shows the high-level technical building blocks (containers) of the system.

**Audience**: Technical stakeholders - developers, architects, operations teams.

**Contains**:

- Containers (deployable/executable units)
- Technology choices for each container
- Communication patterns between containers
- External systems from context diagram

**Important**: In C4 terminology, "container" means a deployable/executable unit like:

- Web application
- Mobile app
- Desktop application
- Database
- File system
- Microservice

NOT Docker containers (though they often align).

**Key Questions Answered**:

- What are the major technical building blocks?
- How do they communicate?
- What technology stack is used?
- What can be deployed independently?

**Example Elements**:

- **Web Containers**: ose-platform-web (Hugo Static Site), ayokoding-web (Hugo Static Site), orca-grid-fe (Next.js)
- **Backend Containers**: orca-grid-be (Spring Boot)
- **CLI Tools**: ayokoding-cli (Go), butler-cli (Go)
- **Test Containers**: orca-grid-fe-e2e (Playwright), orca-grid-be-e2e (Playwright)
- **Infrastructure**: Nx Workspace, Shared Libraries

**Best Practices**:

- One container diagram per software system
- Show major technology choices
- Include all containers that run at runtime
- Clearly label communication protocols (HTTP, gRPC, message queues)
- Color-code containers by type (frontend, backend, database, etc.)

**When to Use**:

- Explaining the overall shape of the system
- Identifying deployment boundaries
- Planning infrastructure and DevOps
- Understanding technology stack decisions

**Example: E-Commerce Microservices Platform Containers**

```mermaid
graph TB
    subgraph "Frontend Applications"
        WEB[Web Application<br/>React SPA]
        MOBILE[Mobile App<br/>React Native]
        ADMIN_UI[Admin Dashboard<br/>Vue.js]
    end

    subgraph "API Gateway Layer"
        API_GW[API Gateway<br/>Kong/AWS API Gateway]
    end

    subgraph "Microservices"
        USER_SVC[User Service<br/>Java Spring Boot<br/>Port 8081]
        PRODUCT_SVC[Product Service<br/>Node.js/Express<br/>Port 8082]
        ORDER_SVC[Order Service<br/>Java Spring Boot<br/>Port 8083]
        PAYMENT_SVC[Payment Service<br/>Python FastAPI<br/>Port 8084]
        NOTIFICATION_SVC[Notification Service<br/>Go<br/>Port 8085]
        INVENTORY_SVC[Inventory Service<br/>Node.js/Express<br/>Port 8086]
    end

    subgraph "Data Stores"
        USER_DB[(User DB<br/>PostgreSQL)]
        PRODUCT_DB[(Product DB<br/>MongoDB)]
        ORDER_DB[(Order DB<br/>PostgreSQL)]
        PAYMENT_DB[(Payment DB<br/>PostgreSQL)]
        INVENTORY_DB[(Inventory DB<br/>MySQL)]
    end

    subgraph "Infrastructure"
        MESSAGE_BUS[Message Bus<br/>RabbitMQ/Kafka]
        CACHE[(Distributed Cache<br/>Redis Cluster)]
    end

    CUSTOMER[Customer]
    MERCHANT[Merchant]
    PAYMENT_EXT[Payment Gateway]
    EMAIL_EXT[Email Service]

    CUSTOMER -->|HTTPS| WEB
    CUSTOMER -->|HTTPS| MOBILE
    MERCHANT -->|HTTPS| ADMIN_UI
    WEB -->|REST/GraphQL<br/>HTTPS| API_GW
    MOBILE -->|REST/GraphQL<br/>HTTPS| API_GW
    ADMIN_UI -->|REST<br/>HTTPS| API_GW

    API_GW -->|HTTP/JSON| USER_SVC
    API_GW -->|HTTP/JSON| PRODUCT_SVC
    API_GW -->|HTTP/JSON| ORDER_SVC
    API_GW -->|HTTP/JSON| INVENTORY_SVC

    ORDER_SVC -->|HTTP/JSON| PAYMENT_SVC
    ORDER_SVC -->|Publish events<br/>AMQP| MESSAGE_BUS
    PAYMENT_SVC -->|Publish events<br/>AMQP| MESSAGE_BUS
    MESSAGE_BUS -->|Subscribe events<br/>AMQP| NOTIFICATION_SVC
    MESSAGE_BUS -->|Subscribe events<br/>AMQP| INVENTORY_SVC

    USER_SVC -->|SQL| USER_DB
    PRODUCT_SVC -->|MongoDB Protocol| PRODUCT_DB
    ORDER_SVC -->|SQL| ORDER_DB
    PAYMENT_SVC -->|SQL| PAYMENT_DB
    INVENTORY_SVC -->|SQL| INVENTORY_DB

    PRODUCT_SVC -->|Get/Set| CACHE
    ORDER_SVC -->|Get/Set| CACHE

    PAYMENT_SVC -->|HTTPS/API| PAYMENT_EXT
    NOTIFICATION_SVC -->|SMTP/API| EMAIL_EXT

    style WEB fill:#0173B2,stroke:#000000,color:#ffffff
    style MOBILE fill:#0173B2,stroke:#000000,color:#ffffff
    style ADMIN_UI fill:#0173B2,stroke:#000000,color:#ffffff
    style API_GW fill:#CC78BC,stroke:#000000,color:#ffffff
    style USER_SVC fill:#029E73,stroke:#000000,color:#ffffff
    style PRODUCT_SVC fill:#029E73,stroke:#000000,color:#ffffff
    style ORDER_SVC fill:#029E73,stroke:#000000,color:#ffffff
    style PAYMENT_SVC fill:#029E73,stroke:#000000,color:#ffffff
    style NOTIFICATION_SVC fill:#029E73,stroke:#000000,color:#ffffff
    style INVENTORY_SVC fill:#029E73,stroke:#000000,color:#ffffff
    style MESSAGE_BUS fill:#DE8F05,stroke:#000000,color:#000000
    style CACHE fill:#DE8F05,stroke:#000000,color:#000000
    style USER_DB fill:#DE8F05,stroke:#000000,color:#000000
    style PRODUCT_DB fill:#DE8F05,stroke:#000000,color:#000000
    style ORDER_DB fill:#DE8F05,stroke:#000000,color:#000000
    style PAYMENT_DB fill:#DE8F05,stroke:#000000,color:#000000
    style INVENTORY_DB fill:#DE8F05,stroke:#000000,color:#000000
    style PAYMENT_EXT fill:#CC78BC,stroke:#000000,color:#ffffff
    style EMAIL_EXT fill:#CC78BC,stroke:#000000,color:#ffffff
```

**Diagram Explanation**:

- **Frontend Applications** (blue): Multiple client applications (web, mobile, admin) serving different user types
- **API Gateway** (purple): Single entry point for all client requests, handles routing, auth, rate limiting
- **Microservices** (teal): Six independently deployable services, each with specific business capability
  - **User Service**: Authentication, user profiles, account management
  - **Product Service**: Product catalog, search, recommendations
  - **Order Service**: Order processing, order lifecycle management
  - **Payment Service**: Payment processing, transaction management
  - **Notification Service**: Email/SMS notifications across all domains
  - **Inventory Service**: Stock management, inventory tracking
- **Databases** (coral): Each microservice owns its database (database-per-service pattern), using different technologies based on needs
- **Message Bus** (orange): Asynchronous communication between services via events
- **Distributed Cache** (orange): Shared Redis cluster for performance optimization
- **Technology Diversity**: Different languages/frameworks chosen per service needs (polyglot architecture)

This diagram reveals the microservices architecture inside the E-Commerce Platform. Key patterns visible:

1. **Database per Service**: Each service has its own database, ensuring loose coupling
2. **API Gateway Pattern**: Centralized entry point for client requests
3. **Event-Driven Communication**: Services communicate asynchronously via message bus
4. **Polyglot Persistence**: Different database technologies (PostgreSQL, MongoDB, MySQL) based on service needs
5. **Independent Deployment**: Each microservice can be deployed, scaled, and updated independently
