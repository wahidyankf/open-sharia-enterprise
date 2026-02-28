import type { Meta, StoryObj } from "@storybook/nextjs-vite";
import { AlertCircle, Terminal, Info } from "lucide-react";

import { Alert, AlertDescription, AlertTitle } from "./alert";

const meta: Meta<typeof Alert> = {
  title: "UI/Alert",
  component: Alert,
  tags: ["autodocs"],
  argTypes: {
    variant: {
      control: "select",
      options: ["default", "destructive"],
    },
  },
};

export default meta;
type Story = StoryObj<typeof Alert>;

export const Default: Story = {
  render: (args) => (
    <Alert {...args}>
      <Terminal className="h-4 w-4" />
      <AlertTitle>Heads up!</AlertTitle>
      <AlertDescription>You can add components and dependencies to your app using the CLI.</AlertDescription>
    </Alert>
  ),
  args: {
    variant: "default",
  },
};

export const Destructive: Story = {
  render: (args) => (
    <Alert {...args}>
      <AlertCircle className="h-4 w-4" />
      <AlertTitle>Error</AlertTitle>
      <AlertDescription>Your session has expired. Please log in again.</AlertDescription>
    </Alert>
  ),
  args: {
    variant: "destructive",
  },
};

export const WithoutIcon: Story = {
  render: (args) => (
    <Alert {...args}>
      <AlertTitle>Note</AlertTitle>
      <AlertDescription>This alert has no icon, just a title and description.</AlertDescription>
    </Alert>
  ),
  args: {
    variant: "default",
  },
};

export const TitleOnly: Story = {
  render: (args) => (
    <Alert {...args}>
      <Info className="h-4 w-4" />
      <AlertTitle>Operation completed successfully.</AlertTitle>
    </Alert>
  ),
  args: {
    variant: "default",
  },
};
