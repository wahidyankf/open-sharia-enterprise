package auth

import (
	"net/http"

	"github.com/gin-gonic/gin"
)

// AdminMiddleware creates a Gin middleware that requires the ADMIN role.
func AdminMiddleware() gin.HandlerFunc {
	return func(c *gin.Context) {
		claimsVal, exists := c.Get(string(ClaimsKey))
		if !exists {
			c.AbortWithStatusJSON(http.StatusUnauthorized, gin.H{"message": "unauthorized"})
			return
		}
		claims, ok := claimsVal.(*Claims)
		if !ok {
			c.AbortWithStatusJSON(http.StatusUnauthorized, gin.H{"message": "unauthorized"})
			return
		}
		if claims.Role != "ADMIN" {
			c.AbortWithStatusJSON(http.StatusForbidden, gin.H{"message": "admin access required"})
			return
		}
		c.Next()
	}
}
