import type { Meta, StoryObj } from "@storybook/nextjs-vite";
import { Toggle } from "./toggle";

const meta = {
  title: "Input/Toggle",
  component: Toggle,
  parameters: { layout: "centered" },
} satisfies Meta<typeof Toggle>;

export default meta;
type Story = StoryObj<typeof meta>;

export const Off: Story = { args: { value: false, onChange: () => {} } };
export const On: Story = { args: { value: true, onChange: () => {} } };
export const WithLabel: Story = { args: { value: false, onChange: () => {}, label: "Enable notifications" } };
export const Disabled: Story = { args: { value: false, onChange: () => {}, disabled: true } };
