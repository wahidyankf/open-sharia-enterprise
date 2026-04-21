import type { Meta, StoryObj } from "@storybook/nextjs-vite";
import { HuePicker, type HueName } from "./hue-picker";

const meta = {
  title: "Foundation/HuePicker",
  component: HuePicker,
  parameters: { layout: "centered" },
} satisfies Meta<typeof HuePicker>;

export default meta;
type Story = StoryObj<typeof meta>;

export const Default: Story = {
  args: { value: "teal" as HueName, onChange: () => {} },
};

export const SageSelected: Story = {
  args: { value: "sage" as HueName, onChange: () => {} },
};
