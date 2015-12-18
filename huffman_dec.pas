{Huffman Decode File}
Program huffman_dec;
Type
  BinFile = File of Byte; {бинарный файл}
  HuffCode = Record {описание кода хаффмана}
               len: Byte;   {длина кода}
               val: Word;   {код}
             End;
Var
  fname: String; {имя файла}
  codes: Array[0..255] of HuffCode; {коды хаффмана}
  numCodes: Integer; {кол-во кодов (кодируемых символов)}
  fsize: LongInt; {размер входного/выходного файла}
  fin, fout: BinFile; {входной/выходной файл}
  b: Byte; {считываемый байт из входного файла}
  bits: Integer; {счетчик входных бит}
  s: Integer; {символ выходного файла}
  val, len: Word; {код и длина кода Хаффмана}

{инициализация переменных}
Procedure Init;
Var
  i: Integer;
Begin
  {очищаем коды Хаффмана}
  numCodes := 0;
  For i := 0 To 255 Do
  Begin
    codes[i].len := 0;
    codes[i].val := 0;
  End;
End;

{чтение магиккода из файла}
Function ReadMagicCode(Var f: BinFile): Word;
Var
  b1, b2: Byte;
Begin
  Read(f, b1);
  Read(f, b2);
  ReadMagicCode := (b1 shl 8) or b2;
End;

{чтение таблицы кодов Хаффмана}
Procedure ReadCodes(Var f: BinFile);
Var
  b1, b2, b3, b4: Byte;
  i: Integer;
Begin
  {чтение ко-лво кодов Хаффмана}
  Read(f, b1);
  If b1 = 0 Then
    numCodes := 256
  Else
    numCodes := b1;
  {чтение кодов}
  For i := 1 To numCodes Do
  Begin
    {чтение кода символа}
    Read(f, b1);
    {чтение длины кода Хаффмана}
    Read(f, b2);
    {чтение кода Хаффмана}
    Read(f, b3);
    Read(f, b4);
    {запись информации о коде}
    With codes[b1] Do
    Begin
      len := b2;
      val := (b3 shl 8) or b4;
    End;
  End;
End;

{вывод кодов Хаффмана для отладки}
Procedure PrintCodes;
Var
  i: Integer;

{вывод кода Хаффмана}
Procedure PrintCode(s: Integer; c: HuffCode);
Var
  i: Integer;
Begin
  Write('symbol: ', s:3, ' len: ', c.len:2, ' code: ');
  For i := c.len - 1 DownTo 0 Do
    Write((c.val shr i) and 1);
  WriteLn;
End;

Begin
  WriteLn('-----');
  WriteLn('Коды Хаффмана:');
  WriteLn('Кол-во символов: ', numCodes);
  For i := 0 To 255 Do
    If codes[i].len > 0 Then
      PrintCode(i, codes[i]);
End;

{функция чтения размера файла}
Function ReadFileSize(Var f: BinFile): LongInt;
Var
  b1, b2, b3, b4: Byte;
Begin
  Read(f, b1);
  Read(f, b2);
  Read(f, b3);
  Read(f, b4);
  ReadFileSize := (b1 shl 24) or (b2 shl 16) or (b3 shl 8) or b4
End;

{функция поиска кода по таблице Хаффмана}
Function FindCode(val, len: Word): Integer;
Var
  i, i1: Integer;
Begin
  i1 := -1;
  For i := 0 To 255 Do
    If (codes[i].len = len) and (codes[i].val = val) Then
    Begin
      i1 := i;
      Break;
    End;
  FindCode := i1;
End;

{основная программа}
Begin
  {вводим имя файла}
  WriteLn('Введите имя файла: ');
  ReadLn(fname);

  {инициализируем переменные}
  Init;

  {АНАЛИЗ ВХОДНОГО ФАЙЛА И ЧТЕНИЕ КОДОВ ХАФФМАНА}

  {открываем входной файл на чтение}
  Assign(fin, fname);
  Reset(fin);

  {читаем магиккод (123,123) из файла и проверяем его}
  If ReadMagicCode(fin) <> ((123 shl 8) or 123) Then
  Begin
    {закрываем файл и выходим из программы}
    WriteLn('ОШИБКА: Неверный магиккод. Файл сжат неизвестным алгоритмом.');
    Close(fin);
    Halt;
  End;

  {считываем коды Хаффмана}
  ReadCodes(fin);
  PrintCodes; {вывод кодов на экран для отладки}

  {читаем размер оригинального файла}
  fsize := ReadFileSize(fin);
  WriteLn('-----');
  WriteLn('Размер оригинального файла: ', fsize);

  {ОСНОВНОЙ ЦИКЛ РАЗАРХИВИРОВАНИЯ ВХОДНОГО ФАЙЛА}

  {открываем выходной файл на запись}
  Assign(fout, Copy(fname, 1, Length(fname) - 4) + '.orig');
  ReWrite(fout);

  bits := 0; {нет обработанных бит, чтобы считать новый байт}
  val := 0; {код}
  len := 0; {длина кода}
  {основной цикл}
  While fsize > 0 Do
  Begin
    {если обработано 8 бит то считываем новый байт из входного файла}
    If bits = 0 Then
    Begin
      If EOF(fin) Then Break; {закончился входной файл}
      Read(fin, b); {считываем байт}
      bits := 8; {очищаем счетчик бит}
    End;
    dec(bits); {уменьшаем счетчик обработанных бит}
    inc(len); {увеличиваем длину кода Хаффмана}
    If len > 16 Then
    Begin
      WriteLn('ОШИБКА: Длина кода Хаффмана не может больше 16 бит');
      Close(fin);
      Close(fout);
      Halt;
    End;
    val := (val shl 1) or ((b shr bits) and 1); {дополняем код битом}
    s := FindCode(val, len); {ищем символ по коду Хаффмана}
    If s >= 0 Then
    Begin
      {символ найден}
      Write(fout, (s and 255)); {запись в файл}
      fsize := fsize - 1; {уменьшаем счетчик записанных байт в выходной файл}
      {очищаем код Хаффмана}
      val := 0;
      len := 0;
    End;
  End;

  {закрыть файлы}
  Close(fin);
  Close(fout);
End.
