import { Header } from "@/components/layout/header";
import { Footer } from "@/components/layout/footer";
import { Hero } from "@/components/landing/hero";
import { SocialIcons } from "@/components/landing/social-icons";

export default function Home() {
  return (
    <>
      <Header />
      <main>
        <Hero />
        <SocialIcons />
      </main>
      <Footer />
    </>
  );
}
