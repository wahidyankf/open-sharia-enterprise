---
title: "Saluran Phoenix"
date: 2026-02-05T00:00:00+07:00
draft: false
weight: 1000017
description: "Komunikasi WebSocket real-time menggunakan Phoenix Channels untuk broadcasting, pelacakan presence, dan sinkronisasi klien"
tags: ["elixir", "phoenix", "channels", "websocket", "real-time", "pubsub", "presence"]
prev: "/id/belajar/software-engineering/programming-languages/elixir/in-the-field/kerangka-phoenix"
next: "/id/belajar/software-engineering/programming-languages/elixir/in-the-field/pola-ecto"
---

## Ketika GenServer PubSub Tidak Cukup

Pola GenServer standar menyediakan messaging tingkat proses, tetapi aplikasi web real-time membutuhkan komunikasi berbasis WebSocket antara server dan klien.

```elixir
# GenServer PubSub untuk komunikasi proses
defmodule DonationBroadcaster do
  # => Broadcasting hanya dalam BEAM VM
  # => Tidak ada komunikasi klien web
  use GenServer
  # => Behavior GenServer OTP

  def start_link(_opts) do
    # => Inisialisasi proses broadcaster
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
    # => Mengembalikan: {:ok, pid}
    # => Proses bernama untuk akses mudah
  end

  def broadcast_donation(campaign_id, amount, donor) do
    # => API publik untuk broadcasting
    # => campaign_id: identifier string
    # => amount: nilai donasi integer
    GenServer.cast(__MODULE__, {:broadcast, campaign_id, amount, donor})
    # => Pesan async ke GenServer
    # => Mengembalikan: :ok segera
  end

  def init(_) do
    # => Setup subscription pada init
    Phoenix.PubSub.subscribe(MyApp.PubSub, "donations")
    # => Subscribe ke topik "donations"
    # => Hanya menerima pesan dari VM yang sama
    # => Tidak bisa mencapai klien eksternal
    {:ok, %{}}
    # => Map state kosong
  end

  def handle_cast({:broadcast, campaign_id, amount, donor}, state) do
    # => Menangani request broadcast
    # => Pattern match tuple: {:broadcast, ...}
    Phoenix.PubSub.broadcast(MyApp.PubSub, "donations", {:new_donation, campaign_id, amount, donor})
    # => Broadcast ke semua proses berlangganan
    # => Pesan: tuple {:new_donation, ...}
    # => Terbatas pada proses server saja
    # => Klien web tidak bisa menerima
    {:noreply, state}
    # => Lanjutkan tanpa reply
  end
end
```

**Keterbatasan GenServer PubSub**:

- **Tidak ada dukungan WebSocket** - Tidak bisa berkomunikasi langsung dengan klien browser
- **Penanganan koneksi manual** - Perlu implementasi WebSocket custom untuk klien eksternal
- **Tidak ada pelacakan presence** - Tidak bisa melacak pengguna yang terhubung
- **Tidak ada abstraksi room** - Manajemen topik manual untuk channel berbeda

## Phoenix Channels - Layer Komunikasi Real-time

Phoenix Channels menyediakan abstraksi WebSocket dengan PubSub bawaan, pelacakan presence, dan manajemen room.

### Dasar-dasar Channel

**Komponen arsitektur**:

- **Channel** - Proses server-side menangani komunikasi WebSocket
- **Socket** - Koneksi antara klien dan server
- **Topic** - Identifier string untuk routing pesan (mis., "campaign:ramadan_2026")
- **PubSub** - Sistem distribusi pesan underlying

```elixir
# Definisi channel untuk kampanye donasi
defmodule MyAppWeb.CampaignChannel do
  # => Modul channel untuk update real-time
  # => Menangani koneksi WebSocket
  use MyAppWeb, :channel
  # => Phoenix Channel behavior
  # => Menyediakan: join/3, handle_in/3, broadcast/3

  @impl true
  def join("campaign:" <> campaign_id, _payload, socket) do
    # => Callback join ketika klien terhubung
    # => Pattern match: prefix "campaign:" + ID
    # => campaign_id: diekstrak dari string topik
    # => socket: Container state koneksi
    send(self(), {:after_join, campaign_id})
    # => Kirim pesan async ke self
    # => Memungkinkan join return cepat
    # => Setup post-join terjadi di handle_info

    {:ok, socket}
    # => Izinkan koneksi
    # => Mengembalikan socket untuk pesan berikutnya
    # => Klien sekarang terhubung
  end
  # => Topik lain otomatis ditolak

  @impl true
  def handle_info({:after_join, campaign_id}, socket) do
    # => Menangani setup post-join
    # => Dipanggil setelah join selesai
    # => campaign_id: dari send(self(), ...)
    campaign = get_campaign_data(campaign_id)
    # => Ambil state kampanye saat ini dari database
    # => Mengembalikan: map dengan detail kampanye

    push(socket, "campaign_data", campaign)
    # => Kirim pesan ke klien terhubung
    # => Nama event: "campaign_data"
    # => Payload: map kampanye
    # => Hanya dikirim ke klien ini

    {:noreply, socket}
    # => Lanjutkan dengan socket yang sama
    # => Tidak perlu perubahan state
  end

  @impl true
  def handle_in("new_donation", %{"amount" => amount, "donor" => donor}, socket) do
    # => Menangani pesan masuk dari klien
    # => Event: "new_donation"
    # => Payload didestruktur: amount, donor
    # => socket: state koneksi saat ini
    campaign_id = socket.assigns.campaign_id
    # => Ekstrak campaign ID dari socket assigns
    # => Disimpan saat join

    process_donation(campaign_id, amount, donor)
    # => Logic bisnis: catat donasi
    # => Database write terjadi di sini

    broadcast(socket, "donation_received", %{
      # => Broadcast ke SEMUA klien pada topik ini
      # => Termasuk pengirim
      amount: amount,
      # => amount: nilai IDR integer
      donor: donor,
      # => donor: nama string
      timestamp: DateTime.utc_now()
      # => Sertakan timestamp server
      # => Memastikan waktu konsisten di semua klien
    })
    # => Semua klien terhubung menerima update
    # => Notifikasi real-time

    {:reply, :ok, socket}
    # => Acknowledgment ke pengirim
    # => Reply: atom :ok
    # => Lanjutkan dengan socket yang sama
  end

  defp get_campaign_data(campaign_id) do
    # => Ambil detail kampanye dari database
    # => campaign_id: identifier string
    %{
      id: campaign_id,
      # => ID sama dengan yang diminta
      goal: 100_000_000,
      # => Target: 100 juta IDR
      raised: 45_000_000,
      # => Progress saat ini: 45 juta IDR
      donors: 1250
      # => Total donatur unik
    }
    # => Mengembalikan: map dengan state kampanye
  end

  defp process_donation(campaign_id, amount, donor) do
    # => Catat donasi dalam database
    # => Update total kampanye
    # => Validasi amount > 0
    :ok
    # => Mengembalikan: :ok pada sukses
  end
end
```

### Konfigurasi Socket

**Setup socket endpoint**:

```elixir
# lib/my_app_web/endpoint.ex
defmodule MyAppWeb.Endpoint do
  # => Konfigurasi endpoint Phoenix
  use Phoenix.Endpoint, otp_app: :my_app
  # => Mendefinisikan behavior endpoint

  socket "/socket", MyAppWeb.UserSocket,
    # => Path endpoint WebSocket: /socket
    # => Route ke: MyAppWeb.UserSocket
    websocket: true,
    # => Aktifkan transport WebSocket
    # => Lebih disukai untuk real-time
    longpoll: false
    # => Nonaktifkan long-polling legacy
    # => Mengurangi overhead server

  # ... konfigurasi endpoint lain
end
```

**Implementasi socket**:

```elixir
# lib/my_app_web/channels/user_socket.ex
defmodule MyAppWeb.UserSocket do
  # => Handler koneksi socket
  use Phoenix.Socket
  # => Phoenix Socket behavior
  # => Menangani lifecycle WebSocket

  channel "campaign:*", MyAppWeb.CampaignChannel
  # => Route topik ke channel
  # => Pola: "campaign:*" cocok dengan semua topik kampanye
  # => Contoh: campaign:ramadan_2026, campaign:education_2026
  # => Handler: MyAppWeb.CampaignChannel

  @impl true
  def connect(_params, socket, _connect_info) do
    # => Callback koneksi
    # => Dipanggil ketika klien pertama kali terhubung
    # => Sebelum join channel apa pun
    # => _params: query params dari klien
    {:ok, socket}
    # => Terima koneksi
    # => socket: state koneksi
    # => Bisa tambahkan autentikasi di sini
  end

  @impl true
  def id(_socket), do: nil
  # => Socket ID untuk identifikasi koneksi
  # => nil: tidak lacak socket berdasarkan ID
  # => Bisa kembalikan user ID untuk pelacakan presence
  # => Format: "user:#{user_id}"
end
```

### Integrasi JavaScript Client-Side

```javascript
// assets/js/socket.js
import { Socket } from "phoenix";
// => Import klien Phoenix Socket
// => Menangani koneksi WebSocket

// Hubungkan ke socket
let socket = new Socket("/socket", {
  // => Buat instance socket
  // => Path: /socket cocok dengan config endpoint
  params: { token: window.userToken },
  // => Token autentikasi
  // => Dikirim ke callback connect/3
});
socket.connect();
// => Establish koneksi WebSocket
// => Operasi async

// Bergabung ke channel kampanye
let channel = socket.channel("campaign:ramadan_2026", {});
// => Buat instance channel
// => Topic: "campaign:ramadan_2026"
// => Payload: objek kosong (tidak ada join params)

channel
  .join()
  // => Coba bergabung ke channel
  // => Memanggil join/3 di server
  .receive("ok", (resp) => {
    // => Tangani join sukses
    // => resp: response dari server
    console.log("Bergabung ke kampanye", resp);
    // => Log pesan sukses
  })
  .receive("error", (resp) => {
    // => Tangani join gagal
    // => resp: detail error
    console.log("Tidak bisa bergabung", resp);
    // => Log pesan error
  });

// Dengarkan event donasi
channel.on("donation_received", (payload) => {
  // => Subscribe ke event
  // => Nama event: "donation_received"
  // => payload: {amount, donor, timestamp}
  console.log(`Donasi baru: ${payload.amount} dari ${payload.donor}`);
  // => Log detail donasi
  // => amount: integer IDR
  // => donor: nama string

  updateCampaignUI(payload);
  // => Update halaman dengan donasi baru
  // => Mencerminkan perubahan real-time
});

// Kirim donasi dari klien
function submitDonation(amount, donor) {
  // => Pengiriman donasi client-side
  // => amount: nilai donasi integer
  // => donor: nama donatur string
  channel
    .push("new_donation", { amount, donor })
    // => Kirim pesan ke server
    // => Event: "new_donation"
    // => Payload: {amount, donor}
    .receive("ok", () => console.log("Donasi terkirim"))
    // => Tangani acknowledgment sukses
    .receive("error", (e) => console.log("Error", e));
  // => Tangani response error
}
```

## Pola Broadcasting

### Broadcasting ke Semua Klien Terhubung

```elixir
defmodule MyAppWeb.CampaignChannel do
  # => Channel kampanye dengan broadcasting
  use MyAppWeb, :channel

  def handle_in("update_campaign", %{"raised" => raised}, socket) do
    # => Menangani request update kampanye dari klien
    # => Event: "update_campaign"
    # => Payload: %{"raised" => jumlah_baru}
    campaign_id = socket.assigns.campaign_id
    # => Ekstrak campaign ID dari state socket
    # => Disimpan saat join

    broadcast(socket, "campaign_updated", %{
      # => Broadcast ke SEMUA klien pada topik ini
      # => Termasuk pengirim (tidak ada self-exclusion)
      campaign_id: campaign_id,
      # => Sertakan identifier kampanye
      raised: raised,
      # => Jumlah raised baru
      timestamp: DateTime.utc_now()
      # => Timestamp server untuk konsistensi
    })
    # => Setiap klien terhubung menerima update
    # => Sinkronisasi real-time

    {:reply, :ok, socket}
    # => Acknowledgment ke pengirim
    # => Reply: atom sukses :ok
  end
end
```

### Broadcasting dari Proses Eksternal

**Pola PubSub untuk broadcast yang dimulai server**:

```elixir
# Broadcast dari mana saja dalam aplikasi Anda
defmodule DonationProcessor do
  # => Processor donasi background
  # => Menangani donasi offline

  def process_bank_transfer(campaign_id, amount, donor) do
    # => Proses donasi offline
    # => campaign_id: identifier string
    # => amount: nilai IDR integer
    # => donor: nama string
    record_donation(campaign_id, amount, donor)
    # => Simpan ke database terlebih dahulu
    # => Memastikan persistence sebelum broadcast

    # Broadcast ke semua klien terhubung
    MyAppWeb.Endpoint.broadcast("campaign:#{campaign_id}", "donation_received", %{
      # => Broadcast melalui endpoint (bukan channel)
      # => Topic: "campaign:ramadan_2026" (diinterpolasi)
      # => Event: "donation_received"
      amount: amount,
      # => Jumlah donasi
      donor: donor,
      # => Nama donatur
      source: "bank_transfer",
      # => Membedakan donasi offline
      timestamp: DateTime.utc_now()
      # => Timestamp server
    })
    # => Semua klien pada kampanye ini menerima update
    # => Tidak perlu proses channel
  end

  defp record_donation(campaign_id, amount, donor) do
    # => Penyisipan database
    # => Insert ke tabel donations
    :ok
    # => Mengembalikan: :ok pada sukses
  end
end

# Penggunaan: Broadcast dari GenServer
defmodule CampaignWorker do
  # => Background worker untuk update periodik
  use GenServer
  # => GenServer behavior

  def handle_info(:refresh_campaign_data, state) do
    # => Menangani trigger refresh periodik
    # => Dijadwalkan dengan Process.send_after
    campaign_id = state.campaign_id
    # => Ekstrak dari state GenServer
    updated_data = fetch_campaign_summary(campaign_id)
    # => Dapatkan statistik kampanye terbaru dari database
    # => Mengembalikan: map dengan total

    MyAppWeb.Endpoint.broadcast!("campaign:#{campaign_id}", "campaign_refreshed", updated_data)
    # => broadcast! raise error jika gagal
    # => Topic: campaign dengan ID
    # => Event: "campaign_refreshed"
    # => Semua klien menerima data fresh

    schedule_refresh()
    # => Jadwalkan refresh berikutnya dalam 1 menit
    {:noreply, state}
    # => Lanjutkan dengan state yang sama
  end

  defp fetch_campaign_summary(campaign_id) do
    # => Agregasi data kampanye dari database
    # => campaign_id: identifier string
    %{raised: 50_000_000, donors: 1350, goal: 100_000_000}
    # => Mengembalikan: statistik kampanye saat ini
  end

  defp schedule_refresh do
    # => Jadwalkan refresh berikutnya
    Process.send_after(self(), :refresh_campaign_data, :timer.minutes(1))
    # => Kirim pesan ke self setelah 1 menit
    # => Trigger handle_info(:refresh_campaign_data, ...)
  end
end
```

## Pelacakan Presence

Phoenix Presence melacak pengguna mana yang terhubung ke channel secara real-time.

### Setup Presence

```elixir
# lib/my_app_web/channels/presence.ex
defmodule MyAppWeb.Presence do
  # => Modul pelacakan presence
  use Phoenix.Presence,
    # => Phoenix Presence behavior
    # => Menangani presence terdistribusi
    otp_app: :my_app,
    # => Nama aplikasi
    pubsub_server: MyApp.PubSub
    # => Backend PubSub untuk sinkronisasi presence
    # => Sinkronisasi antar node
end
```

**Tambahkan ke supervision tree**:

```elixir
# lib/my_app/application.ex
defmodule MyApp.Application do
  # => Modul aplikasi utama
  use Application

  def start(_type, _args) do
    # => Callback start aplikasi
    children = [
      MyApp.Repo,
      # => Pool koneksi database
      MyAppWeb.Endpoint,
      # => Endpoint Phoenix
      MyAppWeb.Presence,
      # => Mulai Presence tracker
      # => Melacak koneksi user
      # ... children lain
    ]

    Supervisor.start_link(children, strategy: :one_for_one, name: MyApp.Supervisor)
    # => Mulai supervisor
    # => strategy: :one_for_one (restart hanya child yang gagal)
  end
end
```

### Melacak User Presence

```elixir
defmodule MyAppWeb.CampaignChannel do
  # => Channel kampanye dengan pelacakan presence
  use MyAppWeb, :channel
  alias MyAppWeb.Presence
  # => Alias modul Presence

  @impl true
  def join("campaign:" <> campaign_id, %{"user_id" => user_id}, socket) do
    # => Join dengan autentikasi user
    # => Pattern match topic: "campaign:" + ID
    # => Payload harus include user_id
    send(self(), :after_join)
    # => Setup post-join async
    # => Memungkinkan join return cepat

    {:ok, assign(socket, :user_id, user_id)}
    # => Simpan user_id dalam socket assigns
    # => Tersedia sebagai socket.assigns.user_id
    # => Mengembalikan: {:ok, updated_socket}
  end

  @impl true
  def handle_info(:after_join, socket) do
    # => Lacak user presence setelah join
    # => Dipanggil setelah join selesai
    push(socket, "presence_state", Presence.list(socket))
    # => Kirim daftar presence saat ini ke user yang join
    # => Event: "presence_state"
    # => Payload: map user terhubung
    # => Menunjukkan siapa yang sudah di sini

    {:ok, _} = Presence.track(socket, socket.assigns.user_id, %{
      # => Lacak presence user ini
      # => Key: user_id dari socket.assigns
      # => Metadata: data custom
      online_at: inspect(System.system_time(:second))
      # => Metadata: timestamp koneksi
      # => Format: string detik sejak epoch
    })
    # => User sekarang terlihat oleh semua klien
    # => Broadcast presence_diff ke semua

    {:noreply, socket}
    # => Lanjutkan dengan socket
  end

  @impl true
  def terminate(_reason, socket) do
    # => Cleanup pada disconnect
    # => Dipanggil ketika klien disconnect
    # => Presence otomatis untracked
    # => Broadcast presence_diff ke klien tersisa
    :ok
  end
end
```

### Penanganan Presence Client-Side

```javascript
// assets/js/socket.js
import { Presence } from "phoenix";
// => Import klien Presence
// => Menangani state dan diff presence

let channel = socket.channel("campaign:ramadan_2026", { user_id: currentUserId });
// => Buat channel dengan user_id
// => currentUserId: dari state aplikasi
let presence = new Presence(channel);
// => Buat instance Presence
// => Bind ke event channel

// Lacak perubahan presence
presence.onSync(() => {
  // => Dipanggil ketika state presence berubah
  // => Dipicu oleh: join, leave, update metadata
  displayUsers(presence.list());
  // => Update UI dengan user saat ini
  // => presence.list() mengembalikan state saat ini
});

function displayUsers(presences) {
  // => Render daftar user
  // => presences: map presence user
  let userList = document.getElementById("user-list");
  // => Dapatkan elemen DOM
  userList.innerHTML = "";
  // => Kosongkan daftar yang ada

  presence.list((id, { metas }) => {
    // => Iterasi user yang hadir
    // => id: user_id (tracking key)
    // => metas: array metadata presence
    let li = document.createElement("li");
    // => Buat elemen list item
    li.textContent = `User ${id} (online at: ${metas[0].online_at})`;
    // => Set text: user ID dan timestamp
    // => metas[0]: entry metadata pertama (biasanya hanya satu)
    userList.appendChild(li);
    // => Tambahkan ke daftar
  });
}

channel
  .join()
  // => Coba bergabung ke channel
  .receive("ok", (resp) => console.log("Bergabung ke kampanye"))
  // => Tangani sukses
  .receive("error", (resp) => console.log("Join gagal", resp));
// => Tangani gagal
```

## Pola Produksi - Dashboard Donasi Live

```elixir
# Sistem pelacakan donasi real-time lengkap
defmodule MyAppWeb.DonationDashboardChannel do
  # => Channel dashboard admin
  # => Monitoring donasi real-time
  use MyAppWeb, :channel
  alias MyAppWeb.Presence
  # => Pelacakan presence untuk admin

  @impl true
  def join("dashboard:live", %{"admin_token" => token}, socket) do
    # => Join dashboard khusus admin
    # => Topic: "dashboard:live"
    # => Payload memerlukan: admin_token
    case verify_admin(token) do
      {:ok, admin_id} ->
        # => Token admin valid
        # => admin_id: identifier admin terautentikasi
        send(self(), {:after_join, admin_id})
        # => Setup post-join async
        {:ok, assign(socket, :admin_id, admin_id)}
        # => Simpan admin ID dalam socket
        # => Izinkan koneksi

      {:error, _reason} ->
        # => Token tidak valid
        # => Autentikasi gagal
        {:error, %{reason: "unauthorized"}}
        # => Tolak koneksi
        # => Klien menerima error
    end
  end

  @impl true
  def handle_info({:after_join, admin_id}, socket) do
    # => Setup post-join untuk admin
    # => admin_id: dari send(self(), ...)
    Presence.track(socket, admin_id, %{
      # => Lacak presence admin
      role: "admin",
      # => Metadata: identifier peran
      joined_at: DateTime.utc_now()
      # => Metadata: timestamp join
    })
    # => Admin terlihat oleh admin lain

    push(socket, "dashboard_state", get_dashboard_data())
    # => Kirim data dashboard awal
    # => Event: "dashboard_state"
    # => Payload: statistik kampanye saat ini, donasi terbaru
    # => Inisialisasi tampilan admin

    {:noreply, socket}
    # => Lanjutkan dengan socket
  end

  @impl true
  def handle_in("request_campaign_update", %{"campaign_id" => campaign_id}, socket) do
    # => Admin request refresh kampanye spesifik
    # => Event: "request_campaign_update"
    # => Payload: campaign_id untuk diambil
    data = get_campaign_details(campaign_id)
    # => Ambil data kampanye terbaru
    # => Mengembalikan: map dengan detail kampanye

    {:reply, {:ok, data}, socket}
    # => Respon hanya ke admin
    # => Reply: tuple {:ok, data}
    # => Tidak broadcast ke semua admin
  end

  defp get_dashboard_data do
    # => Agregasi semua kampanye
    # => Ambil statistik ringkasan
    %{
      total_raised: 250_000_000,
      # => Total di semua kampanye
      # => Jumlah semua raised amounts
      active_campaigns: 8,
      # => Jumlah kampanye berlangsung
      recent_donations: [
        # => Donasi terbaru di semua kampanye
        %{campaign: "ramadan_2026", amount: 1_000_000, donor: "Ahmad"},
        # => Donasi terbaru pertama
        %{campaign: "education_2026", amount: 500_000, donor: "Fatimah"}
        # => Donasi terbaru kedua
      ]
    }
    # => Mengembalikan: map ringkasan dashboard
  end

  defp get_campaign_details(campaign_id) do
    # => Ambil detail kampanye spesifik
    # => campaign_id: identifier string
    %{
      id: campaign_id,
      # => Identifier kampanye
      raised: 45_000_000,
      # => Jumlah raised saat ini
      goal: 100_000_000,
      # => Target goal
      donors: 1250
      # => Jumlah donatur unik
    }
    # => Mengembalikan: map detail kampanye
  end

  defp verify_admin(token) do
    # => Validasi token admin
    # => token: dari join payload
    # => Dalam produksi: cek JWT, sesi database, dll.
    if token == "admin_secret" do
      # => Cek hanya untuk development
      {:ok, "admin_1"}
      # => Mengembalikan: admin ID
    else
      # => Token tidak valid
      {:error, :invalid_token}
      # => Mengembalikan: tuple error
    end
  end
end
```

**Broadcasting ke dashboard dari processor donasi**:

```elixir
defmodule DonationProcessor do
  # => Proses donasi dan broadcast update
  # => Koordinasi database dan update channel

  def process_donation(campaign_id, amount, donor) do
    # => Pemrosesan donasi utama
    # => campaign_id: identifier string
    # => amount: nilai IDR integer
    # => donor: nama string
    record_donation(campaign_id, amount, donor)
    # => Catat donasi dalam database
    # => Memastikan persistence terlebih dahulu

    # Broadcast ke channel kampanye
    MyAppWeb.Endpoint.broadcast("campaign:#{campaign_id}", "donation_received", %{
      # => Notifikasi klien spesifik kampanye
      # => Topic: kampanye spesifik
      # => Semua viewer kampanye menerima update
      amount: amount,
      # => Jumlah donasi
      donor: donor,
      # => Nama donatur
      timestamp: DateTime.utc_now()
      # => Timestamp server
    })
    # => Halaman kampanye update secara real-time

    # Broadcast ke dashboard admin
    MyAppWeb.Endpoint.broadcast("dashboard:live", "new_donation", %{
      # => Notifikasi semua user admin
      # => Topic: dashboard admin
      # => Semua admin menerima notifikasi
      campaign_id: campaign_id,
      # => Kampanye mana yang menerima donasi
      amount: amount,
      # => Jumlah donasi
      donor: donor,
      # => Nama donatur
      timestamp: DateTime.utc_now()
      # => Timestamp server
    })
    # => Dashboard update secara real-time
  end

  defp record_donation(campaign_id, amount, donor) do
    # => Penyisipan database
    # => Insert ke tabel donations
    # => Update total kampanye
    :ok
    # => Mengembalikan: :ok pada sukses
  end
end
```

## Testing Channel

```elixir
# test/my_app_web/channels/campaign_channel_test.exs
defmodule MyAppWeb.CampaignChannelTest do
  # => Suite testing channel
  use MyAppWeb.ChannelCase
  # => Helper testing channel
  # => Menyediakan: socket/2, subscribe_and_join/3, dll.

  setup do
    # => Setup untuk setiap test
    # => Berjalan sebelum setiap test
    {:ok, _, socket} =
      MyAppWeb.UserSocket
      |> socket("user_id", %{some: :assign})
      # => Buat socket test
      # => Assigns: %{some: :assign}
      |> subscribe_and_join(MyAppWeb.CampaignChannel, "campaign:test")
      # => Join topik test
      # => Mengembalikan: {:ok, reply, socket}

    %{socket: socket}
    # => Kembalikan socket untuk test
    # => Tersedia sebagai %{socket: socket} dalam konteks test
  end

  test "broadcast donasi ke semua klien", %{socket: socket} do
    # => Test behavior broadcast
    # => socket: dari setup
    push(socket, "new_donation", %{"amount" => 100_000, "donor" => "Test"})
    # => Kirim pesan ke channel
    # => Event: "new_donation"
    # => Payload: %{"amount" => 100_000, "donor" => "Test"}

    assert_broadcast "donation_received", %{amount: 100_000, donor: "Test"}
    # => Verifikasi broadcast diterima
    # => Event: "donation_received"
    # => Payload cocok: amount dan donor
    # => Semua klien akan menerima ini
  end

  test "replies dengan ok untuk donasi", %{socket: socket} do
    # => Test behavior reply
    # => socket: dari setup
    ref = push(socket, "new_donation", %{"amount" => 100_000, "donor" => "Test"})
    # => Kirim dan tangkap referensi
    # => ref: referensi pesan unik

    assert_reply ref, :ok
    # => Verifikasi pengirim menerima acknowledgment
    # => Reply: atom :ok
    # => Mengkonfirmasi pesan diproses
  end
end
```

## Ringkasan

**Keterbatasan GenServer PubSub**: Tidak ada dukungan WebSocket, penanganan koneksi manual, tidak ada pelacakan presence

**Phoenix Channels menyediakan**: Abstraksi WebSocket, manajemen koneksi otomatis, routing berbasis room

**Pola broadcasting**: Broadcast dipicu klien, broadcast dipicu server dari proses mana pun

**Pelacakan presence**: Pelacakan koneksi user real-time dengan metadata

**Kasus penggunaan produksi**: Dashboard donasi live, update kampanye, monitoring admin

**Manfaat real-time**: Update langsung ke semua klien terhubung, infrastruktur WebSocket scalable

**Langkah selanjutnya**: Jelajahi [distributed Phoenix](/id/belajar/software-engineering/programming-languages/elixir/in-the-field/distributed-phoenix) untuk sistem real-time multi-node, atau [optimasi performa](/id/belajar/software-engineering/programming-languages/elixir/in-the-field/optimasi-performa) untuk scaling channel.
