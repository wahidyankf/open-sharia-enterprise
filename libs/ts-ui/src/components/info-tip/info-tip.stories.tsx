import type { Meta, StoryObj } from "@storybook/nextjs-vite";
import { InfoTip } from "./info-tip";

const meta = {
  title: "Display/InfoTip",
  component: InfoTip,
  parameters: { layout: "centered" },
} satisfies Meta<typeof InfoTip>;

export default meta;
type Story = StoryObj<typeof meta>;

export const Default: Story = {
  args: {
    title: "Rep Range",
    text: "Choose a rep range that challenges you while maintaining proper form. For strength, aim for 3-5 reps. For hypertrophy, 8-12 reps. For endurance, 15+ reps.",
  },
};
