---
title: "Pengaturan Awal"
date: 2026-02-07T00:00:00+07:00
draft: false
weight: 100001
description: "Setup TypeScript di sistem Anda - instalasi Node.js, setup TypeScript compiler, dan program pertama yang berfungsi"
tags: ["typescript", "instalasi", "setup", "beginner", "nodejs"]
---

**Ingin mulai programming di TypeScript?** Panduan pengaturan awal ini akan menginstall dan menjalankan TypeScript di sistem Anda dalam hitungan menit. Di akhir tutorial, Anda akan memiliki TypeScript compiler yang berjalan dan akan menjalankan program type-safe pertama Anda.

Tutorial ini menyediakan cakupan 0-5% - cukup untuk membuat TypeScript bekerja di mesin Anda. Untuk pembelajaran lebih dalam, lanjutkan ke [Mulai Cepat](/id/belajar/software-engineering/programming-languages/typescript/mulai-cepat) (cakupan 5-30%).

## Prasyarat

Sebelum menginstall TypeScript, Anda memerlukan:

- Komputer yang menjalankan Windows, macOS, atau Linux
- Akses Administrator/sudo untuk instalasi
- Terminal/command prompt
- Text editor (VS Code direkomendasikan, atau editor apa pun)
- Skill navigasi command-line dasar

Tidak diperlukan pengalaman TypeScript atau JavaScript sebelumnya - panduan ini dimulai dari nol.

## Tujuan Pembelajaran

Di akhir tutorial ini, Anda akan dapat:

1. **Menginstall** Node.js dan npm di sistem operasi Anda
2. **Menginstall** TypeScript compiler secara global
3. **Membuat** file konfigurasi tsconfig.json dasar
4. **Menulis** program TypeScript pertama dengan type annotations
5. **Mengkompilasi** source code TypeScript ke JavaScript
6. **Menjalankan** JavaScript yang dikompilasi menggunakan Node.js

## Memahami TypeScript: Bahasa, Compiler, Runtime

Sebelum instalasi, pahami komponen kunci TypeScript:

- **TypeScript Language**: Superset dari JavaScript dengan static typing - menambahkan types ke sintaks JavaScript
- **TypeScript Compiler (tsc)**: Mengkonversi TypeScript (.ts) ke JavaScript (.js) - melakukan type checking
- **Node.js Runtime**: Menjalankan JavaScript (TypeScript yang dikompilasi) - browser juga menjalankan compiled output

**Untuk development, Anda perlu**: Node.js + npm (package manager) + TypeScript compiler

## Instalasi Platform-Specific

Pilih sistem operasi Anda dan ikuti langkah-langkah instalasi.

### Instalasi Windows

**Langkah 1: Install Node.js dan npm**

1. Kunjungi [https://nodejs.org/](https://nodejs.org/)
2. Download **versi LTS** (v20+ direkomendasikan - Long Term Support)
3. Jalankan installer `.msi`
4. Ikuti installation wizard:
   - Terima license agreement
   - Simpan default installation directory (`C:\Program Files\nodejs\`)
   - Simpan semua default features yang dicheck (npm, online documentation)
   - Check **Automatically install necessary tools** (menginstall build tools)
   - Klik **Install** (mungkin memerlukan administrator privileges)
5. Klik **Finish** ketika selesai

**Langkah 2: Verifikasi Node.js dan npm**

Buka Command Prompt atau PowerShell (restart jika sebelumnya sudah terbuka):

```cmd
node --version
```

Output yang diharapkan:

```
v20.X.X
```

Check npm (Node Package Manager):

```cmd
npm --version
```

Output yang diharapkan:

```
10.X.X
```

**Langkah 3: Install TypeScript Compiler**

Install TypeScript secara global menggunakan npm:

```cmd
npm install -g typescript
```

Ini menginstall command `tsc` (TypeScript Compiler) secara global.

**Langkah 4: Verifikasi Instalasi TypeScript**

```cmd
tsc --version
```

Output yang diharapkan:

```
Version 5.X.X
```

**Troubleshooting Windows**:

- Jika command `tsc` not found, restart terminal atau komputer untuk reload PATH
- Verifikasi npm global bin directory ada di PATH: `npm config get prefix` harus ada di system PATH
- Manually tambahkan ke PATH: `C:\Users\<YourUsername>\AppData\Roaming\npm`

### Instalasi macOS

**Langkah 1: Install Node.js dan npm**

**Opsi A: Menggunakan Official Installer**

1. Kunjungi [https://nodejs.org/](https://nodejs.org/)
2. Download **versi LTS** untuk macOS
3. Jalankan installer `.pkg`
4. Ikuti installation wizard (memerlukan password untuk system-wide install)
5. Klik **Close** ketika selesai

**Opsi B: Menggunakan Homebrew (Direkomendasikan)**

```bash
brew install node
```

Ini menginstall Node.js dan npm.

**Langkah 2: Verifikasi Instalasi**

Buka Terminal:

```bash
node --version
npm --version
```

Output yang diharapkan: Node v20+ dan npm 10+

**Langkah 3: Install TypeScript Compiler**

```bash
npm install -g typescript
```

**Langkah 4: Verifikasi TypeScript**

```bash
tsc --version
```

Output yang diharapkan: Version 5.X.X

**Troubleshooting macOS**:

- Permission errors: Gunakan `sudo npm install -g typescript` (tidak direkomendasikan) atau konfigurasi npm prefix:

  ```bash
  mkdir ~/.npm-global
  npm config set prefix '~/.npm-global'
  echo 'export PATH=~/.npm-global/bin:$PATH' >> ~/.zshrc
  source ~/.zshrc
  npm install -g typescript
  ```

- Multiple Node versions: Gunakan `nvm` (Node Version Manager) untuk version management

### Instalasi Linux

**Langkah 1: Install Node.js dan npm**

**Ubuntu/Debian**:

```bash
curl -fsSL https://deb.nodesource.com/setup_20.x | sudo -E bash -
sudo apt-get install -y nodejs
```

**Fedora/RHEL/CentOS**:

```bash
curl -fsSL https://rpm.nodesource.com/setup_20.x | sudo bash -
sudo dnf install -y nodejs
```

**Arch Linux**:

```bash
sudo pacman -S nodejs npm
```

**Langkah 2: Verifikasi Instalasi**

```bash
node --version
npm --version
```

**Langkah 3: Install TypeScript Compiler**

```bash
npm install -g typescript
```

Jika permission denied, gunakan salah satu pendekatan ini:

**Opsi A: Gunakan sudo (sederhana tapi tidak ideal)**:

```bash
sudo npm install -g typescript
```

**Opsi B: Konfigurasi npm prefix (direkomendasikan)**:

```bash
mkdir ~/.npm-global
npm config set prefix '~/.npm-global'
echo 'export PATH=~/.npm-global/bin:$PATH' >> ~/.bashrc
source ~/.bashrc
npm install -g typescript
```

**Langkah 4: Verifikasi TypeScript**

```bash
tsc --version
```

**Troubleshooting Linux**:

- Gunakan `nvm` (Node Version Manager) untuk kontrol versi Node.js yang lebih baik
- Check `$PATH` jika `tsc` not found: `echo $PATH | grep npm`

## Program TypeScript Pertama Anda

Mari tulis dan compile program TypeScript pertama dengan type annotations.

### Buat Project Directory

```bash
mkdir -p ~/typescript-projects/hello
cd ~/typescript-projects/hello
```

### Initialize TypeScript Configuration

Buat `tsconfig.json` (konfigurasi TypeScript compiler):

```bash
tsc --init
```

Ini menghasilkan `tsconfig.json` dengan defaults yang masuk akal. Anda juga dapat membuatnya secara manual:

```json
{
  "compilerOptions": {
    "target": "ES2020",
    "module": "commonjs",
    "strict": true,
    "esModuleInterop": true,
    "skipLibCheck": true,
    "forceConsistentCasingInFileNames": true,
    "outDir": "./dist"
  },
  "include": ["src/**/*"],
  "exclude": ["node_modules"]
}
```

**Opsi kunci dijelaskan**:

- `target`: Versi JavaScript untuk dikompilasi (ES2020 modern dan didukung luas)
- `module`: Module system (commonjs untuk Node.js, ESNext untuk environment modern)
- `strict`: Enable semua opsi strict type checking (direkomendasikan)
- `outDir`: Output directory untuk file JavaScript yang dikompilasi

### Tulis Program

Buat directory `src` dan file TypeScript pertama Anda:

```bash
mkdir src
```

Buat `src/hello.ts`:

```typescript
// hello.ts - Program TypeScript pertama dengan type annotations
function greet(name: string): string {
  return `Hello, ${name}!`;
}

const userName: string = "TypeScript";
const message: string = greet(userName);

console.log(message);
```

**Type annotations dijelaskan**:

- `name: string` - parameter type annotation (name harus string)
- `: string` setelah function - return type annotation (function mengembalikan string)
- `const userName: string` - variable type annotation (userName adalah string)

**CRITICAL**: Perhatikan bagaimana types membuat intent eksplisit dan menangkap errors pada compile time.

### Compile Program

TypeScript harus dikompilasi ke JavaScript sebelum eksekusi:

```bash
tsc
```

Ini membaca `tsconfig.json` dan mengkompilasi semua file TypeScript di `src/` ke `dist/`.

**Struktur directory setelah compilation**:

```
~/typescript-projects/hello/
├── tsconfig.json
├── src/
│   └── hello.ts       # TypeScript source
└── dist/
    └── hello.js       # Compiled JavaScript
```

**Compiled output** (`dist/hello.js`):

```javascript
"use strict";
function greet(name) {
  return `Hello, ${name}!`;
}
const userName = "TypeScript";
const message = greet(userName);
console.log(message);
```

Perhatikan: Type annotations dihapus, JavaScript bersih tersisa.

### Jalankan Program

Jalankan JavaScript yang dikompilasi dengan Node.js:

```bash
node dist/hello.js
```

**Output**:

```
Hello, TypeScript!
```

**Apa yang terjadi**:

1. `tsc` mengkompilasi `src/hello.ts` → `dist/hello.js` (dengan type checking)
2. TypeScript memverifikasi semua types benar
3. `node` menjalankan JavaScript output
4. Program mencetak greeting

## Compile dan Run dalam Satu Langkah

Untuk kemudahan, gunakan `ts-node` untuk menjalankan TypeScript langsung tanpa manual compilation:

**Install ts-node**:

```bash
npm install -g ts-node
```

**Jalankan TypeScript langsung**:

```bash
ts-node src/hello.ts
```

Ini mengkompilasi dan menjalankan di memory (tidak ada file `dist/` dibuat).

**Alternatif: Gunakan npm scripts** di `package.json`:

```bash
npm init -y  # Buat package.json
```

Tambahkan scripts ke `package.json`:

```json
{
  "scripts": {
    "build": "tsc",
    "start": "node dist/hello.js",
    "dev": "ts-node src/hello.ts"
  }
}
```

Sekarang jalankan:

```bash
npm run build  # Compile TypeScript
npm start      # Jalankan compiled JavaScript
npm run dev    # Jalankan TypeScript langsung
```

## Contoh Lebih Detail - Type Safety dalam Aksi

Mari lihat bagaimana TypeScript menangkap errors pada compile time. Buat `src/calculator.ts`:

```typescript
// calculator.ts - Mendemonstrasikan type safety TypeScript
interface CalculatorResult {
  value: number;
  operation: string;
}

function add(a: number, b: number): CalculatorResult {
  return {
    value: a + b,
    operation: "addition",
  };
}

function divide(a: number, b: number): CalculatorResult {
  if (b === 0) {
    throw new Error("Cannot divide by zero");
  }
  return {
    value: a / b,
    operation: "division",
  };
}

// Penggunaan type-safe
const result1: CalculatorResult = add(10, 5);
console.log(`${result1.operation}: ${result1.value}`); // addition: 15

const result2: CalculatorResult = divide(10, 2);
console.log(`${result2.operation}: ${result2.value}`); // division: 5

// TypeScript menangkap errors pada compile time
// Uncomment baris ini untuk melihat type errors:

// const badResult = add("10", 5);  // Error: Argument of type 'string' is not assignable to parameter of type 'number'
// const badResult2 = add(10);      // Error: Expected 2 arguments, but got 1
// result1.value = "string";        // Error: Type 'string' is not assignable to type 'number'
```

Compile dan jalankan:

```bash
tsc
node dist/calculator.js
```

**Output**:

```
addition: 15
division: 5
```

**Coba uncomment baris error dan jalankan `tsc`** - compiler akan menangkap errors ini sebelum runtime!

**Benefit type safety**:

- Menangkap type mismatches pada compile time
- Mencegah memanggil functions dengan jumlah argumen salah
- Memastikan object properties memiliki types yang benar
- Menyediakan autocomplete dan inline documentation di editors

## Memahami TypeScript Compilation Flow

TypeScript menggunakan compile-time type checking model:

1. **Write**: Buat file `.ts` dengan type annotations
2. **Compile**: `tsc` checks types dan menghasilkan file `.js`
3. **Execute**: Node.js atau browser menjalankan JavaScript (tidak ada TypeScript runtime)

**Mengapa ini penting**:

- **Type Safety Tanpa Runtime Cost**: Type checking terjadi pada compile time, tidak ada overhead runtime
- **JavaScript Compatibility**: Compiled output adalah JavaScript bersih (berjalan di mana saja)
- **Gradual Adoption**: Campur file TypeScript dan JavaScript dalam project yang sama
- **Tooling**: IDEs memanfaatkan types untuk autocomplete, refactoring, error detection

**Kontras dengan JavaScript** (tidak ada compilation step):

- JavaScript: `.js` → interpreter menjalankan langsung (no type checking)
- TypeScript: `.ts` → compiler checks types → `.js` → runtime executes

## IDE Setup - VS Code (Direkomendasikan)

**Visual Studio Code** menyediakan TypeScript development experience terbaik (dibangun dengan TypeScript):

**Langkah 1: Install VS Code**

Download dari [https://code.visualstudio.com/](https://code.visualstudio.com/)

**Langkah 2: Buka Project Anda**

```bash
code ~/typescript-projects/hello
```

**Langkah 3: TypeScript Support Built-in**

VS Code mencakup TypeScript language support secara otomatis. Anda mendapatkan:

- Instant type checking (red squiggles untuk errors)
- Autocomplete untuk semua methods dan properties yang tersedia
- Go to definition (Cmd/Ctrl + Click)
- Inline documentation on hover
- Safe refactoring (rename symbol across files)

**Langkah 4: Install Extensions Berguna**

- **ESLint** - Linting untuk TypeScript
- **Prettier** - Code formatting
- **Error Lens** - Inline error messages

**Langkah 5: Konfigurasi Auto-Compile on Save**

Buat `.vscode/tasks.json`:

```json
{
  "version": "2.0.0",
  "tasks": [
    {
      "type": "typescript",
      "tsconfig": "tsconfig.json",
      "problemMatcher": ["$tsc"],
      "group": {
        "kind": "build",
        "isDefault": true
      }
    }
  ]
}
```

Sekarang TypeScript mengkompilasi otomatis on save.

## Ringkasan

**Apa yang telah Anda capai**:

- Menginstall Node.js dan npm di sistem operasi Anda
- Menginstall TypeScript compiler secara global
- Membuat konfigurasi tsconfig.json
- Menulis dan mengkompilasi program TypeScript pertama
- Menjalankan compiled JavaScript menggunakan Node.js
- Memahami TypeScript compilation flow
- Setup IDE dengan dukungan TypeScript excellent

**Command kunci yang dipelajari**:

- `node --version` - Check versi Node.js
- `npm --version` - Check versi npm
- `npm install -g typescript` - Install TypeScript secara global
- `tsc --version` - Check versi TypeScript
- `tsc --init` - Initialize konfigurasi TypeScript
- `tsc` - Compile TypeScript ke JavaScript
- `node <file>.js` - Jalankan JavaScript
- `ts-node <file>.ts` - Compile dan jalankan TypeScript langsung

**Skills yang didapat**:

- Instalasi dan verifikasi TypeScript compiler
- Konfigurasi tsconfig.json dasar
- Menulis type-annotated TypeScript code
- Mengkompilasi TypeScript ke JavaScript
- Memahami benefit type safety

## Langkah Selanjutnya

**Siap mempelajari fundamental TypeScript?**

- [Mulai Cepat](/id/belajar/software-engineering/programming-languages/typescript/mulai-cepat) (cakupan 5-30%) - Sentuh semua konsep inti TypeScript dalam tur cepat

**Ingin fundamental komprehensif?**

- [By-Example Tutorial](/id/belajar/software-engineering/programming-languages/typescript/by-example) - Belajar melalui contoh beranotasi lengkap

**Ingin memahami filosofi desain TypeScript?**

- [Ikhtisar](/id/belajar/software-engineering/programming-languages/typescript/ikhtisar) - Mengapa TypeScript ada dan kapan menggunakannya

## Troubleshooting Masalah Umum

### "tsc: command not found" atau "tsc is not recognized"

**Masalah**: Terminal tidak mengenali TypeScript compiler.

**Solusi**:

- Verifikasi instalasi: `npm list -g typescript`
- Check npm global bin directory: `npm config get prefix`
- Tambahkan npm global bin ke PATH:
  - **Windows**: `C:\Users\<YourUsername>\AppData\Roaming\npm`
  - **macOS/Linux**: `~/.npm-global/bin` atau `/usr/local/bin`
- Restart terminal setelah PATH changes

### Permission denied saat menginstall TypeScript

**Masalah**: npm install gagal dengan EACCES error.

**Solusi**:

- **Jangan gunakan sudo** (security risk dan menyebabkan permission issues)
- Konfigurasi npm untuk menggunakan user directory:

  ```bash
  mkdir ~/.npm-global
  npm config set prefix '~/.npm-global'
  echo 'export PATH=~/.npm-global/bin:$PATH' >> ~/.bashrc
  source ~/.bashrc
  npm install -g typescript
  ```

### TypeScript compiler error: "Cannot find name 'console'"

**Masalah**: Type definitions untuk Node.js hilang.

**Solusi**:

```bash
npm install --save-dev @types/node
```

Tambahkan ke `tsconfig.json`:

```json
{
  "compilerOptions": {
    "types": ["node"]
  }
}
```

### Compiled JavaScript tidak berjalan

**Masalah**: `node dist/hello.js` gagal atau menghasilkan errors.

**Solusi**:

- Verifikasi compilation berhasil: Check ada file `dist/hello.js`
- Check versi Node.js: Pastikan mendukung target di tsconfig.json
- Verifikasi tidak ada TypeScript compilation errors: Jalankan `tsc` dan check output
- Check module system: Jika menggunakan ES modules, tambahkan `"type": "module"` ke package.json

### IDE tidak menampilkan TypeScript errors

**Masalah**: VS Code atau IDE lain tidak menampilkan type errors.

**Solusi**:

- Reload VS Code: Cmd/Ctrl + Shift + P → "Reload Window"
- Check versi TypeScript di IDE: Bottom right corner di VS Code
- Verifikasi `tsconfig.json` di project root
- Install TypeScript di project: `npm install --save-dev typescript`

## Sumber Daya Lebih Lanjut

**Official TypeScript Documentation**:

- [TypeScript Handbook](https://www.typescriptlang.org/docs/handbook/intro.html) - Panduan TypeScript lengkap
- [TypeScript Playground](https://www.typescriptlang.org/play) - Online TypeScript editor dan compiler
- [TypeScript Release Notes](https://www.typescriptlang.org/docs/handbook/release-notes/overview.html) - Apa yang baru di setiap versi

**Development Tools**:

- [VS Code](https://code.visualstudio.com/) - Best TypeScript IDE (dibangun dengan TypeScript)
- [ts-node](https://typestrong.org/ts-node/) - Jalankan TypeScript langsung
- [tsc-watch](https://www.npmjs.com/package/tsc-watch) - Auto-compile on file changes

**Learning Resources**:

- [DefinitelyTyped](https://github.com/DefinitelyTyped/DefinitelyTyped) - Type definitions untuk JavaScript libraries
- [TypeScript Deep Dive](https://basarat.gitbook.io/typescript/) - Free online book
- [Type Challenges](https://github.com/type-challenges/type-challenges) - Praktik type-level programming

**Community**:

- [TypeScript Discord](https://discord.gg/typescript) - Real-time chat community
- [Stack Overflow - TypeScript](https://stackoverflow.com/questions/tagged/typescript) - Q&A community
- [/r/typescript](https://www.reddit.com/r/typescript/) - Reddit community
