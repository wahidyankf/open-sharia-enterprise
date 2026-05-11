import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { Badge } from "./badge";

const meta: Meta<typeof Badge> = {
  title: "Components/Badge",
  component: Badge,
  tags: ["autodocs"],
  argTypes: {
    variant: {
      control: "select",
      options: ["default", "outline", "secondary", "destructive"],
    },
    size: {
      control: "select",
      options: ["sm", "md"],
    },
    hue: {
      control: "select",
      options: ["teal", "sage", "honey", "terracotta", "plum", "sky"],
    },
  },
  args: {
    children: "label",
    variant: "default",
    size: "sm",
  },
};

export default meta;

type Story = StoryObj<typeof Badge>;

export const AllVariants: Story = {
  render: () => (
    <div className="flex flex-wrap gap-2">
      <Badge variant="default" hue="teal">
        default
      </Badge>
      <Badge variant="outline" hue="teal">
        outline
      </Badge>
      <Badge variant="secondary">secondary</Badge>
      <Badge variant="destructive">destructive</Badge>
    </div>
  ),
};

export const AllHues: Story = {
  render: () => (
    <div className="flex flex-wrap gap-2">
      {(["teal", "sage", "honey", "terracotta", "plum", "sky"] as const).map((hue) => (
        <Badge key={hue} variant="default" hue={hue}>
          {hue}
        </Badge>
      ))}
    </div>
  ),
};

export const HoneyOutline: Story = {
  args: {
    variant: "outline",
    hue: "honey",
    children: "⚗️ Pre-Alpha",
  },
};

export const TealDefault: Story = {
  args: {
    variant: "default",
    hue: "teal",
    children: "workout",
  },
};
