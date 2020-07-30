package fi.vm.sade.valintalaskenta.domain;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.zip.GZIPInputStream;
import java.util.zip.GZIPOutputStream;
import org.apache.commons.io.IOUtils;

public class GzipUtil {
  public static String dekoodaa(byte[] gzipattuData) {
    try {
      return IOUtils.toString(new GZIPInputStream(new ByteArrayInputStream(gzipattuData)));
    } catch (IOException e) {
      throw new RuntimeException(e);
    }
  }

  public static byte[] enkoodaa(String data) {
    try {
      ByteArrayOutputStream b = new ByteArrayOutputStream();
      GZIPOutputStream g = new GZIPOutputStream(b);
      IOUtils.write(data.getBytes(), g);
      IOUtils.closeQuietly(g);
      return b.toByteArray();
    } catch (Exception e) {
      throw new RuntimeException(e);
    }
  }
}
