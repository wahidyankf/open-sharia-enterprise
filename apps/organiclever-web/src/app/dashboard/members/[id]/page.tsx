"use client";

import { useState, useEffect, useCallback } from "react";
import { useRouter } from "next/navigation";
import { useAuth } from "../../../contexts/auth-context";
import { Card, CardContent, CardHeader, CardTitle } from "@/components/ui/card";
import { Github, Mail } from "lucide-react";
import { Navigation } from "@/components/Navigation";
import Breadcrumb from "@/components/Breadcrumb";

type Member = {
  id: number;
  name: string;
  role: string;
  email?: string;
  github: string;
};

const MemberInfo = ({ member }: { member: Member }) => (
  <div className="space-y-4">
    <div>
      <strong>Role:</strong> {member.role}
    </div>
    {member.email && (
      <div className="flex items-center">
        <Mail className="h-4 w-4 mr-2" />
        <span>{member.email}</span>
      </div>
    )}
    <div className="flex items-center">
      <Github className="h-4 w-4 mr-2" />
      <a
        href={`https://github.com/${member.github}`}
        target="_blank"
        rel="noopener noreferrer"
        className="text-primary hover:underline"
      >
        {member.github}
      </a>
    </div>
  </div>
);

const MemberCard = ({ member }: { member: Member }) => (
  <Card>
    <CardHeader>
      <CardTitle>{member.name}</CardTitle>
    </CardHeader>
    <CardContent>
      <MemberInfo member={member} />
    </CardContent>
  </Card>
);

const MemberDetailContent = ({ member }: { member: Member }) => (
  <div className="container mx-auto px-6 py-8">
    <Breadcrumb />
    <div className="mb-6 flex justify-between items-center">
      <h1 className="text-2xl font-bold">Member Details</h1>
    </div>
    <MemberCard member={member} />
  </div>
);

export default function MemberDetailPage({ params }: { params: { id: string } }) {
  const { isAuthenticated, logout } = useAuth();
  const [member, setMember] = useState<Member | null>(null);
  const router = useRouter();

  const fetchMember = useCallback(async () => {
    try {
      const response = await fetch(`/api/members/${params.id}`);
      if (!response.ok) {
        throw new Error("Failed to fetch member");
      }
      const data = await response.json();
      setMember(data);
    } catch (error) {
      console.error("Error fetching member:", error);
      router.push("/dashboard/members");
    }
  }, [params.id, router]);

  useEffect(() => {
    if (!isAuthenticated) {
      router.push("/login");
    } else {
      fetchMember();
    }
  }, [isAuthenticated, router, fetchMember]);

  if (!isAuthenticated || !member) {
    return null;
  }

  return (
    <div className="flex h-screen bg-gray-100">
      <Navigation logout={logout} />
      <div className="flex-1 flex flex-col overflow-hidden">
        <main className="flex-1 overflow-x-hidden overflow-y-auto bg-gray-100">
          <MemberDetailContent member={member} />
        </main>
      </div>
    </div>
  );
}
