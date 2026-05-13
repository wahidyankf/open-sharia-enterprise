import { Header } from "@/contexts/app-shell/presentation/header";
import { Footer } from "@/contexts/app-shell/presentation/footer";
import { Hero } from "@/contexts/landing/presentation/hero";
import { SocialIcons } from "@/contexts/landing/presentation/social-icons";

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
