import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { Input } from "./input";

const meta: Meta<typeof Input> = {
  title: "Forms/Input",
  component: Input,
  tags: ["autodocs"],
  argTypes: {
    type: {
      control: "select",
      options: ["text", "email", "password", "number", "search", "tel", "url"],
    },
    placeholder: { control: "text" },
    disabled: { control: "boolean" },
    "aria-invalid": { control: "boolean" },
  },
  args: {
    type: "text",
    placeholder: "Enter text...",
    disabled: false,
  },
  decorators: [
    (Story) => (
      <div className="w-80">
        <Story />
      </div>
    ),
  ],
};

export default meta;

type Story = StoryObj<typeof Input>;

export const Default: Story = {};

export const Interactive: Story = {
  args: {
    placeholder: "Type something...",
    type: "text",
  },
};

export const WithValue: Story = {
  name: "With Value",
  args: {
    defaultValue: "john.doe@example.com",
    type: "email",
  },
};

export const TypeEmail: Story = {
  name: "Type / Email",
  args: { type: "email", placeholder: "you@example.com" },
};

export const TypePassword: Story = {
  name: "Type / Password",
  args: { type: "password", placeholder: "Enter password" },
};

export const TypeNumber: Story = {
  name: "Type / Number",
  args: { type: "number", placeholder: "0" },
};

export const TypeSearch: Story = {
  name: "Type / Search",
  args: { type: "search", placeholder: "Search..." },
};

export const TypeFile: Story = {
  name: "Type / File",
  args: { type: "file" },
};

export const Disabled: Story = {
  args: {
    disabled: true,
    placeholder: "Disabled input",
  },
};

export const DisabledWithValue: Story = {
  name: "Disabled With Value",
  args: {
    disabled: true,
    defaultValue: "Read-only value",
  },
};

export const Invalid: Story = {
  name: "Aria Invalid",
  args: {
    "aria-invalid": true,
    defaultValue: "bad-email",
    type: "email",
  },
};

export const AllStates: Story = {
  name: "All States",
  render: () => (
    <div className="flex w-80 flex-col gap-3">
      <Input placeholder="Default" />
      <Input defaultValue="With value" />
      <Input disabled placeholder="Disabled" />
      <Input disabled defaultValue="Disabled with value" />
      <Input aria-invalid={true} defaultValue="Invalid state" />
    </div>
  ),
};
