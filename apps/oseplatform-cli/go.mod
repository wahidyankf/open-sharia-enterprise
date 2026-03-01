module github.com/wahidyankf/open-sharia-enterprise/apps/oseplatform-cli

go 1.26

require (
	github.com/spf13/cobra v1.10.2
	github.com/wahidyankf/open-sharia-enterprise/libs/golang-commons v0.0.0-00010101000000-000000000000
)

require (
	github.com/inconshreveable/mousetrap v1.1.0 // indirect
	github.com/spf13/pflag v1.0.9 // indirect
)

replace github.com/wahidyankf/open-sharia-enterprise/libs/golang-commons v0.0.0-00010101000000-000000000000 => ../../libs/golang-commons
