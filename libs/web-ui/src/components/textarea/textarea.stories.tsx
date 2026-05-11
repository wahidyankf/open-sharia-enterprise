import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { Textarea } from "./textarea";

const meta: Meta<typeof Textarea> = {
  title: "Forms/Textarea",
  component: Textarea,
  tags: ["autodocs"],
  argTypes: {
    placeholder: { control: "text" },
    disabled: { control: "boolean" },
    rows: { control: "number" },
  },
  args: {
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

type Story = StoryObj<typeof Textarea>;

export const Default: Story = {};

export const Placeholder: Story = {
  args: {
    placeholder: "Write here…",
  },
};

export const Disabled: Story = {
  args: {
    disabled: true,
    placeholder: "Cannot edit",
  },
};

export const WithRows: Story = {
  args: {
    rows: 5,
    placeholder: "5-row textarea",
  },
};
