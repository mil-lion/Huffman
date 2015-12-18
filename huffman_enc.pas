{Huffman Encode File}
Program huffman_enc;
Type
  BinFile = File of Byte; {бинарный файл}
  HuffCode = Record {описание кода хаффмана}
               len: Byte;   {длина кода}
               val: Word;   {код}
             End;
  Node = Record {узел дерева Хаффмана}
           freq: LongInt;   {частота вхождения}
           parent: Integer; {родительский элемент узла: -1 - не назначен (свободный узел)}
           child: Array[0..1] of Integer; {дочерние элементы}
         End;

Var
  fname: String; {имя входного файла}
  huffTree: Array[0..511] of Node; {дерево Хаффмана}
  psCode: Integer; {псевдокод символа из дерева Хаффмана (256..511)}
  codes: Array[0..255] of HuffCode; {коды хаффмана}
  numCodes: Integer; {кол-во кодов (кодируемых символов)}
  fsize_in, fsize_out: LongInt; {размер входного/выходного файла}

{инициализация переменных}
Procedure Init;
Var
  i: Integer;
Begin
  {очищаем дерево Хаффмана}
  psCode := 255;
  For i := 0 To 511 Do
  Begin
    huffTree[i].freq := 0;
    huffTree[i].parent := -1;
    huffTree[i].child[0] := -1;
    huffTree[i].child[1] := -1;
  End;
  {очищаем коды Хаффмана}
  numCodes := 0;
  For i := 0 To 255 Do
  Begin
    codes[i].len := 0;
    codes[i].val := 0;
  End;
End;

{подсчет частоты вхождения каждого символа}
Procedure FileStat(fname: String);
Var
  f: BinFile;
  b: Byte;
  i: Integer;
Begin
  {открываем файл на чтение}
  Assign(f, fname);
  Reset(f);
  {получаем размер входного файла}
  fsize_in := FileSize(f);
  {подсчитываем статистику}
  While Not EOF(f) Do
  Begin
    Read(f, b);
    huffTree[b].freq := huffTree[b].freq + 1;
  End;
  {закрываем файл}
  Close(f);
  {считаем сколько символов встречается во входном файле}
  numCodes := 0;
  For i := 0 To 255 Do
    If huffTree[i].freq > 0 Then
      inc(numCodes);
End;

Procedure PrintStat;
Var
  i: Integer;
Begin
  WriteLn('-----');
  WriteLn('Статистика по входному файлу ', fname, ':');
  WriteLn('Размер входного файла: ', fsize_in);
  WriteLn('Частота вхождения символов в файле:');
  For i := 0 To 255 Do
    WriteLn(i:3, ': ', huffTree[i].freq:10);
  WriteLn('Кол-во встреченных в файле символов: ', numCodes);
End;

{процедура формирования дерева Хаффмана}
Procedure BuildTree;
Var
  i0, i1: Integer; {индексы узла дерева}

{функция поиска символа с минимальной частотой вхождения среди свободных узлов}
Function FindMinFreq: Integer;
Var
  i, i1: Integer;
  min: LongInt;
Begin
  i1 := -1;
  min := 2147483647;//MaxLongInt;
  For i := 0 To psCode Do
    If (huffTree[i].parent = -1) And (huffTree[i].freq > 0) And (huffTree[i].freq < min) Then
    Begin
      i1 := i;
      min := huffTree[i].freq;
    End;
  {записываем результат поиска}
  FindMinFreq := i1;
End;

Begin
  {делаем пока не закончатся свободные узлы}
  While true Do
  Begin
    {ищем символ с минимальной частотой вхождения среди свободных узлов}
    i0 := FindMinFreq;
    If i0 < 0 Then Break; {если не нашли свободный узел то выход из цикла}
    {убираем узел из свободных узлов}
    huffTree[i0].parent := -2;
    {ищем символ с минимальной частотой вхождения среди свободных узлов}
    i1 := FindMinFreq;
    If i1 < 0 Then Break; {если не нашли свободный узел то выход из цикла}
    {убираем узел из свободных узлов}
    huffTree[i1].parent := -2;
    {создаем новый псевдоузел}
    inc(psCode);
    huffTree[i0].parent := psCode;
    huffTree[i1].parent := psCode;
    huffTree[psCode].freq := huffTree[i0].freq + huffTree[i1].freq;
    huffTree[psCode].child[0] := i0;
    huffTree[psCode].child[1] := i1;
  End;
End;

{вывод дерева Хаффмана для отладки}
Procedure PrintTree;

{вывод информации об узле}
Procedure PrintNode(level, c: Integer);
Var
  i: Integer;
Begin
  {выводим информацию об узле}
  For i := 1 To level * 2 Do Write(' '); {делаем отступ по уровню}
  WriteLn('code: ', c:3, ' freq: ', huffTree[c].freq:10);
  If c >= 256 Then
  Begin
    {печатаем дочерние элементы}
    PrintNode(level + 1, huffTree[c].child[0]);
    PrintNode(level + 1, huffTree[c].child[1]);
    //For i := 0 To 511 Do
    //  If huffTree[i].parent = c Then
    //    PrintNode(level + 1, i);
  End;
End;

Begin
  WriteLn('-----');
  WriteLn('Дерево Хаффмана:');
  PrintNode(0, psCode);
End;

{процедура формирования кода Хаффмана по дереву}
Procedure BuildCode(symb, val, len: Integer); {символ, код, уровень}
Var
  v: Word;
  l: Byte;
Begin
  //WriteLn('symb: ', symb, ' val: ', val, ' len: ', len);
  If symb < 256 Then
  Begin
    {конечный узел дерева}
    codes[symb].len := len;
    codes[symb].val := val;
  End
  Else
  Begin
    l := len + 1;
    {первый дочерний элемент с битом '0'}
    v := (val shl 1) or 0; {добавляем к коду 0}
    BuildCode(huffTree[symb].child[0], v, l);
    {второй дочерний элемент с битом '1'}
    v := (val shl 1) or 1; {добавляем к коду 1}
    BuildCode(huffTree[symb].child[1], v, l);
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

{процедура записи магического кода в начало файла}
Procedure WriteMagicCode(Var f: BinFile);
Begin
  Write(f, 123);
  Write(f, 123);
End;

{процедура записи таблицы кодов Хаффмана в выходной файл}
Procedure WriteHuffTable(Var f: BinFile);
Var
  i: Integer;
Begin
  {записываем кол-во символов, если 0 - то это 256 символов}
  Write(f, numCodes and 255);
  {записываем элементы таблицы}
  For i := 0 To 255 Do
  If codes[i].len > 0 Then
  Begin
    {записываем код символа}
    Write(f, (i and 255));
    {записыааем длину кода Хаффмана}
    Write(f, codes[i].len);
    {записываем код Хаффмана}
    Write(f, ((codes[i].val shr 8) and 255)); {старший байт кода}
    Write(f, (codes[i].val and 255));         {младший байт кода}
  End;
End;

{запись в файл размера входного файла (4байта)}
Procedure WriteFileSize(Var f: BinFile);
Begin
  Write(f, ((fsize_in shr 24) and 255));
  Write(f, ((fsize_in shr 16) and 255));
  Write(f, ((fsize_in shr  8) and 255));
  Write(f, (fsize_in and 255));
End;

{процедура архивирования файла}
Procedure ArchiveFile(fname: String);
Var
  fin, fout: BinFile; {входной/выходной файл}
  b: Byte; {считываемый байт}
  buffwr: Byte; {байт для записи в файл}
  bits: Integer; {счетчик бит}

{процедура записи кода в выходной файл}
Procedure putCode(var f: BinFile; c: HuffCode);
Var
  i: Integer;
  bit: Byte;
Begin
  {записываем по одному биту кода}
  For i := c.len - 1 DownTo 0 Do
  Begin
    {проверяем кол-во заполненных бит}
    If bits = 8 Then
    Begin
      {байт заполнен}
      Write(fout, buffwr); {записываем байт в файл}
      bits := 0;   {сбрасываем счетчик бит}
      buffwr := 0; {очищаем байт для записи}
    End;
    {записываем бит в буфер}
    bit := (c.val shr i) and 1;
    buffwr := buffwr or (bit shl (7 - bits));
    inc(bits); {увеличиваем счетчик бит}
  End;
End;

Begin
  {открываем входной файл}
  Assign(fin, fname);
  Reset(fin);

  {открываем выходной файл}
  Assign(fout, fname + '.huf');
  ReWrite(fout);

  {запись магического кода в начало файла}
  WriteMagicCode(fout);

  {записываем таблицу Хаффмана в выходной файл}
  WriteHuffTable(fout);

  {записсываем размер входного файла}
  WriteFileSize(fout);

  {инициализация переменных}
  bits := 0;   {сбрасываем счетчик бит}
  buffwr := 0; {очищаем байт для записи}

  {архивируем входной файл}
  While Not EOF(fin) Do
  Begin
    Read(fin, b);
    putCode(fout, codes[b]);
  End;

  {запись незаполненного байта в файл}
  If bits > 0 Then
    Write(fout, buffwr);

  {размер выходного файла}
  fsize_out := FileSize(fout);
  WriteLn('Размер выходного файла: ', fsize_out);

  {закрываем файлы}
  Close(fin);
  Close(fout);
End;

{процедура расчета размера выходного файла}
Procedure CalculateOutFileSize;
Var
  fsize: LongInt;
  i: Integer;
Begin
  {  2 байта магиккод 
   + 1 байт кол-во кодов 
   + 4 байта на 1 символ кода Хаффмана 
   + 4 байта длина вх. файла}
  fsize := (2 + 1 + numCodes * 4 + 4) * 8;
  For i := 0 To 255 Do
    fsize := fsize + (codes[i].len * huffTree[i].freq);
  {округляем размер до байта}
  WriteLn('Рассчитанный размер выходного файла: ', Trunc((fsize + 7)/8));
End;

{основная программа}
Begin
  {вводим имя файла}
  WriteLn('Введите имя файла: ');
  ReadLn(fname);

  {АНАЛИЗ ВХОДНОГО ФАЙЛА И ФОРМИРОВАНИЕ КОДОВ ХАФФМАНА}
  {инициализируем переменные}
  Init;

  {анализируем входной файл}
  FileStat(fname);
  PrintStat; {печатаем статистику для отладки}

  {строим дерево Хаффмана}
  BuildTree;
  PrintTree; {выводим дерево Хаффмана для отладки}

  {формируем коды Хаффмана}
  BuildCode(psCode, 0, 0);
  PrintCodes; {выводим коды Хаффмана для отладки}

  {ОСНОВНОЙ ЦИКЛ АРХИВИРОВАНИЯ ВХОДНОГО ФАЙЛА}
  ArchiveFile(fname);

  WriteLn('Процент сжатия файла: ', (fsize_out / fsize_in * 100):7:2, '%');
  CalculateOutFileSize;
End.
