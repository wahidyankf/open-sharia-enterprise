package titles

import (
	"fmt"
	"os"
	"strings"

	"gopkg.in/yaml.v3"
)

// Config holds title generation configuration
type Config struct {
	Overrides      map[string]string `yaml:"overrides"`
	LowercaseWords []string          `yaml:"lowercase_words"`
}

// LoadConfig loads configuration from a YAML file
func LoadConfig(filePath string) (*Config, error) {
	// Check if file exists
	if _, err := os.Stat(filePath); os.IsNotExist(err) {
		// Return empty config if file doesn't exist (not an error)
		return &Config{
			Overrides:      make(map[string]string),
			LowercaseWords: []string{},
		}, nil
	}

	// Read file
	data, err := os.ReadFile(filePath)
	if err != nil {
		return nil, fmt.Errorf("failed to read config file: %w", err)
	}

	// Parse YAML
	var config Config
	if err := yaml.Unmarshal(data, &config); err != nil {
		return nil, fmt.Errorf("failed to parse YAML: %w", err)
	}

	// Initialize maps if nil
	if config.Overrides == nil {
		config.Overrides = make(map[string]string)
	}
	if config.LowercaseWords == nil {
		config.LowercaseWords = []string{}
	}

	// Normalize override keys to lowercase for case-insensitive matching
	normalizedOverrides := make(map[string]string)
	for key, value := range config.Overrides {
		normalizedOverrides[strings.ToLower(key)] = value
	}
	config.Overrides = normalizedOverrides

	// Normalize lowercase words to lowercase
	for i, word := range config.LowercaseWords {
		config.LowercaseWords[i] = strings.ToLower(word)
	}

	return &config, nil
}

// HasOverride checks if a word has an override (case-insensitive)
func (c *Config) HasOverride(word string) bool {
	_, exists := c.Overrides[strings.ToLower(word)]
	return exists
}

// GetOverride returns the override value for a word (case-insensitive)
func (c *Config) GetOverride(word string) string {
	return c.Overrides[strings.ToLower(word)]
}

// IsLowercaseWord checks if a word should be lowercase (case-insensitive)
func (c *Config) IsLowercaseWord(word string) bool {
	lowerWord := strings.ToLower(word)
	for _, lw := range c.LowercaseWords {
		if lw == lowerWord {
			return true
		}
	}
	return false
}
