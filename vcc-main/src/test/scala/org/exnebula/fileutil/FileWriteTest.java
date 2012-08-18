/*
 * Copyright (C) 2008-2012 - Thomas Santana <tms@exnebula.org>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>
 */
package org.exnebula.fileutil;

import org.junit.Test;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

public class FileWriteTest {

  @Test
  public void test1() throws Exception {
    FileWriter fileWriter = new FileWriter();
    String content = "Test\n\u1230";
    File file = makeTemp();
    fileWriter.storeFileFromStream(new ByteArrayInputStream(content.getBytes()), file);
    String valueRead = readFileContent(file);
    assertEquals(content, valueRead);
  }

  private String readFileContent(File file) throws IOException {
    FileInputStream is = new FileInputStream(file);
    byte[] buffer = new byte[10240];
    int count = is.read(buffer);
    if (count < 0) {
      fail("File is empty");
    } else if (count < buffer.length) {
      return new String(buffer, 0, count, "UTF-8");
    } else {
      fail("File is longer than buffer");
    }
    return null;
  }

  private File makeTemp() throws IOException {
    File file = File.createTempFile("file", "txt");
    file.deleteOnExit();
    return file;
  }

}