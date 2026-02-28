import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { Input } from "./input";

const meta: Meta<typeof Input> = {
  title: "UI/Input",
  component: Input,
  tags: ["autodocs"],
  argTypes: {
    type: {
      control: "select",
      options: ["text", "email", "password", "number", "search", "tel", "url"],
    },
    disabled: { control: "boolean" },
    placeholder: { control: "text" },
  },
};

export default meta;
type Story = StoryObj<typeof Input>;

export const Default: Story = {
  args: {
    type: "text",
    placeholder: "Enter text...",
  },
};

export const Email: Story = {
  args: {
    type: "email",
    placeholder: "you@example.com",
  },
};

export const Password: Story = {
  args: {
    type: "password",
    placeholder: "Enter your password",
  },
};

export const WithValue: Story = {
  args: {
    type: "text",
    defaultValue: "Hello, World!",
  },
};

export const Disabled: Story = {
  args: {
    type: "text",
    placeholder: "Disabled input",
    disabled: true,
    defaultValue: "Cannot edit this",
  },
};

export const Search: Story = {
  args: {
    type: "search",
    placeholder: "Search...",
  },
};

export const Number: Story = {
  args: {
    type: "number",
    placeholder: "0",
    min: 0,
    max: 100,
  },
};
