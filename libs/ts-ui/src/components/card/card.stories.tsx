import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { Button } from "../button/button";
import { Card, CardContent, CardDescription, CardFooter, CardHeader, CardTitle } from "./card";

const meta: Meta<typeof Card> = {
  title: "Layout/Card",
  component: Card,
  tags: ["autodocs"],
  parameters: {
    layout: "centered",
  },
  decorators: [
    (Story) => (
      <div className="w-96">
        <Story />
      </div>
    ),
  ],
};

export default meta;

type Story = StoryObj<typeof Card>;

export const Default: Story = {
  render: () => (
    <Card>
      <CardHeader>
        <CardTitle>Card title</CardTitle>
        <CardDescription>Card description providing additional context.</CardDescription>
      </CardHeader>
      <CardContent>
        <p className="text-sm text-muted-foreground">Card body content goes here.</p>
      </CardContent>
      <CardFooter>
        <Button variant="outline" className="w-full">
          Action
        </Button>
      </CardFooter>
    </Card>
  ),
};

export const HeaderOnly: Story = {
  name: "Header Only",
  render: () => (
    <Card>
      <CardHeader>
        <CardTitle>Header only card</CardTitle>
        <CardDescription>A card showing just the header section.</CardDescription>
      </CardHeader>
    </Card>
  ),
};

export const ContentOnly: Story = {
  name: "Content Only",
  render: () => (
    <Card>
      <CardContent className="pt-6">
        <p className="text-sm text-muted-foreground">A card with only content and no header or footer.</p>
      </CardContent>
    </Card>
  ),
};

export const WithoutFooter: Story = {
  name: "Without Footer",
  render: () => (
    <Card>
      <CardHeader>
        <CardTitle>No footer card</CardTitle>
        <CardDescription>This card does not have a footer section.</CardDescription>
      </CardHeader>
      <CardContent>
        <p className="text-sm text-muted-foreground">Content without an action footer.</p>
      </CardContent>
    </Card>
  ),
};

export const WithMultipleActions: Story = {
  name: "With Multiple Actions",
  render: () => (
    <Card>
      <CardHeader>
        <CardTitle>Multiple actions</CardTitle>
        <CardDescription>Card with primary and secondary actions in the footer.</CardDescription>
      </CardHeader>
      <CardContent>
        <p className="text-sm text-muted-foreground">Review the details before proceeding.</p>
      </CardContent>
      <CardFooter className="gap-2">
        <Button variant="outline" className="flex-1">
          Cancel
        </Button>
        <Button className="flex-1">Confirm</Button>
      </CardFooter>
    </Card>
  ),
};

export const AsFormCard: Story = {
  name: "As Form Card",
  render: () => (
    <Card>
      <CardHeader>
        <CardTitle>Create account</CardTitle>
        <CardDescription>Enter your information to create a new account.</CardDescription>
      </CardHeader>
      <CardContent className="grid gap-4">
        <div className="grid gap-1.5">
          <label htmlFor="card-name" className="text-sm font-medium">
            Name
          </label>
          <input
            id="card-name"
            placeholder="John Doe"
            className="h-9 rounded-md border border-input bg-transparent px-3 py-1 text-sm outline-none placeholder:text-muted-foreground"
          />
        </div>
        <div className="grid gap-1.5">
          <label htmlFor="card-email" className="text-sm font-medium">
            Email
          </label>
          <input
            id="card-email"
            type="email"
            placeholder="john@example.com"
            className="h-9 rounded-md border border-input bg-transparent px-3 py-1 text-sm outline-none placeholder:text-muted-foreground"
          />
        </div>
      </CardContent>
      <CardFooter>
        <Button className="w-full">Create account</Button>
      </CardFooter>
    </Card>
  ),
};

export const TitleOnly: Story = {
  name: "Title Without Description",
  render: () => (
    <Card>
      <CardHeader>
        <CardTitle>Title without description</CardTitle>
      </CardHeader>
      <CardContent>
        <p className="text-sm text-muted-foreground">Card content when there is no description.</p>
      </CardContent>
    </Card>
  ),
};
