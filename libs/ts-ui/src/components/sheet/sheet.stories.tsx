import type { Meta, StoryObj } from "@storybook/nextjs-vite";
import { Sheet } from "./sheet";

const meta = {
  title: "Feedback/Sheet",
  component: Sheet,
  parameters: { layout: "centered" },
} satisfies Meta<typeof Sheet>;

export default meta;
type Story = StoryObj<typeof meta>;

export const WithTitle: Story = {
  args: {
    title: "Settings",
    onClose: () => {},
    children: <p>Sheet content goes here.</p>,
  },
};

export const LongContent: Story = {
  args: {
    title: "Long Content Sheet",
    onClose: () => {},
    children: (
      <div>
        {Array.from({ length: 8 }).map((_, i) => (
          <p key={i}>Paragraph {i + 1}: Lorem ipsum dolor sit amet.</p>
        ))}
      </div>
    ),
  },
};
