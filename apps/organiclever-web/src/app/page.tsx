"use client";

import Link from "next/link";
import { Button } from "@/components/ui/button";
import { Card, CardContent, CardHeader, CardTitle } from "@/components/ui/card";
import { Alert, AlertDescription, AlertTitle } from "@/components/ui/alert";
import { TentTree, AlertTriangle, Code, GitBranch, Trello, BarChart3, LucideIcon } from "lucide-react";
import { useAuth } from "./contexts/auth-context";

interface HeaderProps {
  isAuthenticated: boolean;
}

const Header: React.FC<HeaderProps> = ({ isAuthenticated }) => (
  <header className="px-4 lg:px-6 h-14 flex items-center">
    <div className="max-w-[1440px] w-full mx-auto flex justify-between items-center">
      <Link className="flex items-center justify-center" href="/">
        <TentTree className="h-6 w-6" />
        <span className="ml-2 text-lg font-bold">Organic Lever</span>
      </Link>
      <nav className="flex gap-4 sm:gap-6">
        <Link className="text-sm font-medium hover:underline underline-offset-4" href="#">
          About
        </Link>
        {isAuthenticated ? (
          <Link className="text-sm font-medium hover:underline underline-offset-4" href="/dashboard">
            Dashboard
          </Link>
        ) : (
          <Link className="text-sm font-medium hover:underline underline-offset-4" href="/login">
            Login
          </Link>
        )}
      </nav>
    </div>
  </header>
);

const HeroSection: React.FC = () => (
  <section className="w-full py-12 md:py-24 lg:py-32 xl:py-48">
    <div className="container px-4 md:px-6">
      <div className="flex flex-col items-center space-y-4 text-center">
        <div className="space-y-2">
          <h1 className="text-3xl font-bold tracking-tighter sm:text-4xl md:text-5xl lg:text-6xl/none">
            Boost Your Software Team&apos;s Productivity
          </h1>
          <p className="mx-auto max-w-[700px] text-gray-500 md:text-xl dark:text-gray-400">
            Organic Lever helps software engineering and product management teams track, analyze, and improve
            performance with powerful insights and intuitive tools.
          </p>
        </div>
        <div className="flex flex-col sm:flex-row space-y-4 sm:space-y-0 sm:space-x-4">
          <Button size="lg" asChild>
            <Link href="/login">Get Started</Link>
          </Button>
          <Button size="lg" variant="outline">
            Learn More
          </Button>
        </div>
      </div>
    </div>
  </section>
);

interface FeatureCardProps {
  icon: LucideIcon;
  title: string;
  description: string;
}

const FeatureCard: React.FC<FeatureCardProps> = ({ icon: Icon, title, description }) => (
  <Card>
    <CardHeader>
      <Icon className="h-6 w-6 mb-2" />
      <CardTitle>{title}</CardTitle>
    </CardHeader>
    <CardContent>
      <p>{description}</p>
    </CardContent>
  </Card>
);

const FeaturesSection: React.FC = () => (
  <section className="w-full py-12 md:py-24 lg:py-32 bg-gray-100 dark:bg-gray-800">
    <div className="container px-4 md:px-6">
      <h2 className="text-3xl font-bold tracking-tighter sm:text-4xl md:text-5xl text-center mb-8 md:mb-12">
        Key Features
      </h2>
      <div className="grid gap-6 sm:grid-cols-2 lg:grid-cols-4">
        <FeatureCard
          icon={Code}
          title="Code Metrics"
          description="Track code quality, commit frequency, and pull request metrics in real-time."
        />
        <FeatureCard
          icon={GitBranch}
          title="Version Control Insights"
          description="Gain insights into your Git workflow and optimize your branching strategies."
        />
        <FeatureCard
          icon={Trello}
          title="Sprint Performance"
          description="Visualize sprint progress, velocity, and burndown charts for better planning."
        />
        <FeatureCard
          icon={BarChart3}
          title="Productivity Analytics"
          description="Gain actionable insights to boost individual and team productivity in software development."
        />
      </div>
    </div>
  </section>
);

const TeamSection: React.FC = () => (
  <section className="w-full py-12 md:py-24 lg:py-32">
    <div className="container px-4 md:px-6">
      <h2 className="text-3xl font-bold tracking-tighter sm:text-4xl md:text-5xl text-center mb-8 md:mb-12">
        Empower Your Software Teams
      </h2>
      <div className="grid gap-6 md:grid-cols-2">
        <Card>
          <CardHeader>
            <CardTitle>Software Engineering</CardTitle>
          </CardHeader>
          <CardContent>
            <p>
              Track code commits, pull requests, and deployment frequencies. Identify bottlenecks in your development
              process and optimize your workflow for maximum efficiency.
            </p>
          </CardContent>
        </Card>
        <Card>
          <CardHeader>
            <CardTitle>Product Management</CardTitle>
          </CardHeader>
          <CardContent>
            <p>
              Monitor feature progress, sprint velocities, and backlog health. Make data-driven decisions to prioritize
              tasks and deliver value to your customers faster.
            </p>
          </CardContent>
        </Card>
      </div>
    </div>
  </section>
);

const Footer: React.FC = () => (
  <footer className="px-4 lg:px-6 py-6 w-full shrink-0 border-t">
    <div className="max-w-[1440px] w-full mx-auto flex flex-col sm:flex-row justify-between items-center">
      <p className="text-xs text-gray-500 dark:text-gray-400 mb-4 sm:mb-0">
        © 2024 Organic Lever. All rights reserved.
      </p>
      <nav className="flex gap-4 sm:gap-6">
        <Link className="text-xs hover:underline underline-offset-4" href="#">
          Terms of Service
        </Link>
        <Link className="text-xs hover:underline underline-offset-4" href="#">
          Privacy
        </Link>
      </nav>
    </div>
  </footer>
);

export default function Home() {
  const { isAuthenticated } = useAuth();

  return (
    <div className="flex flex-col min-h-screen">
      <Alert variant="destructive" className="rounded-none max-w-[1440px] mx-auto">
        <AlertTriangle className="h-4 w-4" />
        <AlertTitle>Work in Progress</AlertTitle>
        <AlertDescription>
          Organic Lever is currently under development. Features and functionality may change.
        </AlertDescription>
      </Alert>
      <Header isAuthenticated={isAuthenticated} />
      <main className="flex-1">
        <div className="max-w-[1440px] mx-auto px-4">
          <HeroSection />
          <FeaturesSection />
          <TeamSection />
        </div>
      </main>
      <Footer />
    </div>
  );
}
