defmodule Circuits.I2C.SHT3x do
  @moduledoc """
  A driver for the SHT3x-DIS Humidity and Temperature Sensor

  An example:
  ```elixir
  iex> alias Circuits.I2C
  iex> alias Circuits.I2C.SHT3x
  iex> {:ok, bus} = I2C.open("i2c-1")
  
  iex> sht3x = SHT3x.new(bus, repeatability: :high)
  iex> SHT3x.status(sht3x)
  iex> SHT3x.serial_number(sht3x)
  iex> SHT3x.single_shot(sht3x)
  ```
  """

  # Sensirion SHT3x Datasheet:
  # https://www.sensirion.com/fileadmin/user_upload/customers/sensirion/Dokumente/2_Humidity_Sensors/Datasheets/Sensirion_Humidity_Sensors_SHT3x_Datasheet_digital.pdf

  alias Circuits.I2C

  @type repeatability() :: :high | :medium | :low
  @type frequency() :: :"0.5" | :"1" | :"2" | :"4" | :"10"
  @type temperature_unit() :: :C | :F

  @type sht3x_command() :: <<_::8_2>> | [0x00..0xff]
  @type checksum_value() :: 0x00..0xff
  @type word() :: <<_::8_2>>

  # @repeatabilities [:high, :medium, :low]
  # @frequencies [:"0.5", :"1", :"2", :"4", :"10"]

  @address_a 0x44
  @address_b 0x45

  @command_read_status [0xf3, 0x2d]
  @command_clear_status [0x30, 0x41]
  @command_read_serial_number [0x37, 0x80]
  @command_soft_reset [0x30, 0xa2]
  @command_break [0x30, 0x93]
  @command_heater_enable [0x30, 0x6d]
  @command_heater_disable [0x30, 0x66]
  
  @command_mode_single %{
    # clock_stretching / repeatability
    false: {0x24, {0x00, 0x0b, 0x16}},
    true:  {0x2C, {0x06, 0x0d, 0x10}},
  }
  @command_mode_periodic %{
    # frequency / repeatability
    :"0.5" => {0x20, {0x32, 0x24, 0x2f}},
    :"1"   => {0x21, {0x30, 0x26, 0x2d}},
    :"2"   => {0x22, {0x36, 0x20, 0x2b}},
    :"4"   => {0x23, {0x34, 0x22, 0x29}},
    :"10"  => {0x27, {0x37, 0x21, 0x2a}},
  }
  @command_fetch_data [0xe0, 0x00]

  @single_mode_delay_ms {16, 7, 5}

  defstruct bus: nil,
            address: @address_a,
            repeatability: :high,
            clock_stretching: false,
            art: false,
            frequency: :"4",
            temperature_unit: :C

  @type t() :: %__MODULE__{
    bus: I2C.bus(),
    address: I2C.address(),
    repeatability: repeatability(),
    clock_stretching: boolean(),
    art: boolean(),
    frequency: frequency(),
    temperature_unit: temperature_unit(),
  }

  @spec new(I2C.bus, Keyword.t) :: t
  def new(bus, opts \\ []) when is_reference(bus) and is_list(opts) do
    opts = Keyword.put(opts, :bus, bus)
    opts = maybe_address_b(opts)
    struct(__MODULE__, opts)
  end

  @spec status(t) :: {:ok, map} | {:error, term}
  def status(%__MODULE__{} = sht3x) do
    with {:ok, [raw_status]} <- write_read(sht3x, @command_read_status, 1) do
      <<pending_alerts    :: size(1),
        _reserved1        :: size(1),
        heater_status     :: size(1),
        _reserved2        :: size(1),
        humidity_alert    :: size(1),
        temperature_alert :: size(1),
        _reserved3        :: size(5),
        reset_detected    :: size(1),
        _reserved4        :: size(2),
        command_status    :: size(1),
        checksum_status   :: size(1) >> = raw_status

      status = %{
        pending_alerts: pending_alerts,
        heater_status: heater_status,
        humidity_alert: humidity_alert,
        temperature_alert: temperature_alert,
        reset_detected: reset_detected,
        command_status: command_status,
        checksum_status: checksum_status,
      }
      {:ok, status}
    end
  end

  @spec clear_status(t) :: :ok
  def clear_status(%__MODULE__{} = sht3x) do
    write(sht3x, @command_clear_status, 1)
  end

  @spec serial_number(t) :: {:ok, integer} | {:error, term}
  def serial_number(%__MODULE__{} = sht3x) do
    with {:ok, [w1, w2]} <- write_read(sht3x, @command_read_serial_number, 2) do
      <<sn :: size(32)>> = w1 <> w2
      {:ok, sn}
    end
  end

  @spec soft_reset(t) :: :ok
  def soft_reset(%__MODULE__{} = sht3x) do
    write(sht3x, @command_soft_reset, 2)
  end

  @spec break(t) :: :ok
  def break(%__MODULE__{} = sht3x) do
    write(sht3x, @command_break, 1)
  end

  @spec heater(t, boolean) :: :ok
  def heater(%__MODULE__{} = sht3x, true) do
    write(sht3x, @command_heater_enable, 1)
  end
  def heater(%__MODULE__{} = sht3x, false) do
    write(sht3x, @command_heater_disable, 1)
  end

  @spec temporary_enable_heater(t, pos_integer) :: :ok
  def temporary_enable_heater(%__MODULE__{} = sht3x, delay) do
    with :ok <- heater(sht3x, true),
         :ok <- Process.sleep(delay),
         :ok <- heater(sht3x, false)
    do
      :ok
    end
  end

  @spec single_shot(t) :: {:ok, {temperature :: float, humidity :: float}} | {:error, term}
  def single_shot(%__MODULE__{clock_stretching: clock_stretching,
                              repeatability: repeatability} = sht3x) do
    {msb, lsb} = Map.fetch!(@command_mode_single, clock_stretching)
    lsb = by_repeatability(lsb, repeatability)
    delay = by_repeatability(@single_mode_delay_ms, repeatability)
    with {:ok, [temp, humid]} <- write_read(sht3x, [msb, lsb], 2, delay) do
      {:ok, {temperature(temp, sht3x), humidity(humid)}}
    end
  end

  def start_periodic(%__MODULE__{repeatability: repeatability} = sht3x) do
    frequency = get_frequency(sht3x)
    {msb, lsb} = Map.fetch!(@command_mode_periodic, frequency)
    lsb = by_repeatability(lsb, repeatability)
    write(sht3x, [msb, lsb], 1)
  end

  def fetch_periodic(%__MODULE__{} = sht3x) do
    with {:ok, data} <- write_read(sht3x, @command_fetch_data, 16, 1) do
      measurements =
        for [temp, humid] <- Enum.chunk_every(data, 2),
            temp != nil, humid != nil do
          {temperature(temp, sht3x), humidity(humid)}
        end
      {:ok, measurements}
    end
  end

  def stop_periodic(%__MODULE__{} = sht3x) do
    :ok = SHT3x.break(sht3x)
    :ok = SHT3x.soft_reset(sht3x)
    SHT3x.fetch_periodic(sht3x)
    :ok
  end

  #-----------------------------------------------------------------------------

  defp maybe_address_b(opts) do
    opts =
      case Keyword.get(opts, :address_b) do
        true -> Keyword.put(opts, :address, @address_b)
        _    -> opts
      end
    Keyword.drop(opts, [:address_b])
  end

  defp unpack_words(data, acc \\ [])
  defp unpack_words(<<>>, acc), do: {:ok, :lists.reverse(acc)}
  defp unpack_words(<<0xff, 0xff, _crc :: size(8), rest :: bitstring>>, acc) do
    unpack_words(rest, [nil | acc])
  end
  defp unpack_words(<<word :: bitstring-size(16),
                      crc  :: size(8),
                      rest :: bitstring>>, acc) do
    with :ok <- verify_checksum(word, crc) do
      unpack_words(rest, [word | acc])
    end
  end

  @spec write(t, iodata) :: :ok
  defp write(%__MODULE__{bus: bus, address: address}, command) do
    I2C.write(bus, address, command)
  end

  @spec write(t, iodata, pos_integer) :: :ok
  defp write(%__MODULE__{} = sht3x, command, delay) do
    with :ok <- write(sht3x, command) do
      Process.sleep(delay)
    end
  end

  @spec write_read(t, iodata, pos_integer) :: {:ok, [word]} | {:error, term}
  defp write_read(%__MODULE__{bus: bus, address: address},
                  command, words_to_read) do
    with {:ok, bytes} <- I2C.write_read(bus, address, command, 3*words_to_read),
         {:ok, words} <- unpack_words(bytes)
    do
      {:ok, words}
    end
  end

  @spec write_read(t, iodata, pos_integer, pos_integer)
        :: {:ok, [word]} | {:error, term}
  defp write_read(%__MODULE__{bus: bus, address: address} = sht3x,
                  command, words_to_read, delay) do
    with :ok <- write(sht3x, command, delay),
         {:ok, bytes} <- I2C.read(bus, address, 3*words_to_read),
         {:ok, words} <- unpack_words(IO.inspect(bytes, label: "^^^^^", limit: :infinity, binaries: :as_binaries))
    do
      {:ok, words}
    end
  end

  @spec by_repeatability({any, any, any}, repeatability) :: any
  defp by_repeatability({high, _, _}, :high  ), do: high
  defp by_repeatability({_, med, _ }, :medium), do: med
  defp by_repeatability({_, _, low }, :low   ), do: low

  defp get_frequency(%__MODULE__{art: true}), do: :"4"
  defp get_frequency(%__MODULE__{frequency: frequency}), do: frequency

  @spec verify_checksum(binary, checksum_value)
        :: :ok | {:error, :checksum_mismatch}
  def verify_checksum(data, expected) do
    # https://github.com/TattdCodeMonkey/crc
    model = %{
      name: "CRC-8/SENSIRON",
      width: 8,
      poly: 0x31,
      init: 0xff,
      refin: false,
      refout: false,
      xorout: 0x00,
    }
    case CRC.crc(model, data) == expected do
      false -> IO.inspect({data, expected}); {:error, :checksum_mismatch}
      true  -> :ok
    end
  end

  defp temperature(word, %__MODULE__{temperature_unit: temperature_unit}),
    do: temperature(word, temperature_unit)
  defp temperature(<<word :: size(16)>>, :C), do: -45 + (175 * word / 0xffff)
  defp temperature(<<word :: size(16)>>, :F), do: -49 + (315 * word / 0xffff)

  defp humidity(<<word :: size(16)>>), do: 100 * word / 0xffff
end
