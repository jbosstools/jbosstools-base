/*******************************************************************************
 * Copyright (c) 2007 Exadel, Inc. and Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Exadel, Inc. and Red Hat, Inc. - initial API and implementation
 ******************************************************************************/ 
package org.jboss.tools.common.util;

import java.io.*;
import java.util.*;
import java.util.jar.*;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

import org.jboss.tools.common.CommonPlugin;

public final class FileUtil {

    public FileUtil() {}

    public static String readFile(File f) {
        if(!f.isFile()) return "";
    	ReadBytes bs = readBytes(f);
    	if(bs == null) return "";
        String encoding = getEncoding(bs.bs);
        if(encoding == null) return new String(bs.bs, 0, bs.length);
        try {
        	return new String(bs.bs, 0, bs.length, encoding);
        } catch (Exception e) {
        	return new String(bs.bs, 0, bs.length);
        }
    }

    public static ReadBytes readBytes(File f) {
        if(!f.isFile()) return null;
        try {
            FileInputStream fr = new FileInputStream(f);
            BufferedInputStream br = new BufferedInputStream(fr);
            int l = (int)f.length();
            byte[] bs = new byte[l];
            l = br.read(bs, 0, l);
            br.close();
            fr.close();
            return new ReadBytes(bs, l);
        } catch (Exception e) {
        	return null;
        }
    }
    
    static class ReadBytes {
    	byte[] bs;
    	int length;
    	
    	ReadBytes(byte[] bs, int l) {
    		this.bs = bs;
    		length = l;
    	}
    }

    public static String readFile(File f, String encoding) {
    	ReadBytes bs = readBytes(f);
    	if(bs == null) return null;
        try {
            return new String(bs.bs, 0, bs.length, encoding);
        } catch (Exception e) {
        	return null;
        }
    }

    public static boolean isTextFile(File f, int length) {
        if(!f.isFile()) return false;
        try {
            FileReader fr = new FileReader(f);
            BufferedReader br = new BufferedReader(fr);
            int l = (int)f.length();
            if(l > length) l = length;
            char[] cs = new char[l];
            br.read(cs, 0, l);
            br.close();
            fr.close();
            return isText(new String(cs));
        } catch (Exception e) {
            return false;
        }
    }

    public static boolean isText(String body) {
        if(body == null) return false;
        int l = body.length();
        for (int i = 0; i < l; i++) {
            char c = body.charAt(i);
            if(((int)c) < 32 && c != '\n' && c != '\r' && c != 't') return false;
        }
        return true;
    }

    public static String readStream(InputStream is) {
        StringBuffer sb = new StringBuffer("");
        try {
            byte[] b = new byte[4096];
            while(true) {
                int l = is.read(b, 0, b.length);
                if(l < 0) break;
                sb.append(new String(b, 0, l));
            }
            is.close();
        } catch (Exception e) {
        	CommonPlugin.getPluginLog().logError(e);
        }
        return sb.toString();
    }

    public static boolean writeFile(File f, String value) {
    	if(value == null) return false;
    	String encoding = getEncoding(value);
    	if(value.startsWith("<?xml")) {
    		String s = validateEncoding(encoding, "UTF-8");
    		if(encoding == null) {
    			encoding = s;
    		} else if(s == null || !s.equals(encoding)) {
    			return false;
    		}
    	}
    	if(encoding == null) return writeFileDefault(f, value);
    	return writeFile(f, value, encoding);
    }

    public static boolean writeFileDefault(File f, String value) {
        try {
            try {
                if(f.isFile() && !isSameFile(f)) f.delete();
                if(!f.exists()) f.createNewFile();
            } catch (Exception e) {}
            PrintWriter pw = new PrintWriter(new FileWriter(f));
            pw.print(value);
            pw.flush();
            pw.close();
            return true;
        } catch (Exception e) {
            return false;
        }
    }


    public static boolean copyFile(File source, File dest, boolean mkdirs) {
        return copyFile(source, dest, mkdirs, true);
    }

    public static boolean copyFile(File source, File dest) {
        return copyFile(source, dest, false, true);
    }

    public static boolean copyFile(File source, File dest, boolean mkdirs, boolean overwrite) {
        if (mkdirs) dest.getParentFile().mkdirs();
        if(!source.isFile()) return false;
        try {
            if(dest.isFile() && !isSameFile(dest)) dest.delete();
            if(dest.isFile() && !overwrite) return false;
            if(!dest.exists()) dest.createNewFile();
        } catch (Exception e) {
        }
        InputStream is = null;
        OutputStream os = null;
        try {
            is = new BufferedInputStream(new FileInputStream(source), 16 * 1024);
            os = new BufferedOutputStream(new FileOutputStream(dest), 16 * 1024);
            copyStream(is, os);
            return true;
        } catch (Exception e) {
        	CommonPlugin.getPluginLog().logError(e);
            return false;
        } finally {
            try {
                if (is != null) is.close();
            } catch (IOException e) {
            }
            try {
                if (os != null) os.close();
            } catch (IOException e) {
            }
        }
    }

    public static boolean updateFile(File source, File dest, boolean mkdirs) {
        if(!source.isFile()) return false;
        if(dest.isFile() && (dest.lastModified()<source.lastModified())) {
            dest.delete();
        }
        return copyFile(source, dest, mkdirs);
    }

    public static void copyStream(InputStream is, OutputStream os) throws IOException {
        byte[] buffer = new byte[1<<14];
        while (true) {
            int r = is.read(buffer);
            if (r > 0) {
                os.write(buffer, 0, r);
            } else if (r == -1) break;
        }
        os.flush();
    }

    public static void clear(File f) {
        if(!f.isDirectory()) return;
        File[] fs = f.listFiles();
        if(fs != null) for (int i = 0; i < fs.length; i++) remove(fs[i]);
    }

    public static void remove(File f) {
        if(f.isFile()) f.delete();
        if(!f.isDirectory()) return;
        File[] fs = f.listFiles();
        if(fs != null) for (int i = 0; i < fs.length; i++) remove(fs[i]);
        f.delete();
    }

    public static boolean isSameFile(File f) {
        if(!f.exists()) return false;
        String fn = f.getName();
        try {
           String cn = f.getCanonicalFile().getName();
           return fn.equals(cn);
        } catch (Exception e) {
            return false;
        }
    }

    public static void copyDir(File from, File to) {
        copyDir(from, to, false);
    }

    public static void copyDir(File from, File to, boolean mkdirs) {
        copyDir(from, to, mkdirs, true);
    }

    public static void copyDir(File from, File to, boolean mkdirs, boolean includeSubdirs) {
        copyDir(from, to, includeSubdirs, mkdirs, false);
    }

    public static void copyDir(File from, boolean includeSubdirs, File to) {
        copyDir(from, to, includeSubdirs, false, false);
    }

    public static void copyDir(File from, File to, boolean includeSubdirs, boolean mkdirs, boolean overwriteOnlyOlderFiles) {
    	copyDir(from, to, includeSubdirs, mkdirs, overwriteOnlyOlderFiles, null);
    }

    public static void copyDir(File from, File to, boolean includeSubdirs, boolean mkdirs, boolean overwriteOnlyOlderFiles, FileFilter filter) {
        if(filter != null && !filter.accept(from)) return;
        if (mkdirs) to.mkdirs();
        if(from == null || !from.isDirectory() || !to.isDirectory()) return;
        File[] fs = from.listFiles();
        if(fs == null) return;
        for (int i = 0; i < fs.length; i++) {
            String n = fs[i].getName();
            File c = new File(to, n);
            if (fs[i].isDirectory() && !includeSubdirs) continue;
        	if(filter != null && !filter.accept(new File(from, n))) continue;

            if(fs[i].isDirectory()) {
                c.mkdirs();
                copyDir(fs[i], c, includeSubdirs, mkdirs, overwriteOnlyOlderFiles, filter);
            } else if (overwriteOnlyOlderFiles && fs[i].isFile() && c.isFile()) {
                copyFile(fs[i], c, false, c.lastModified() < fs[i].lastModified());
            } else {
                copyFile(fs[i], c);
            }
        }
    }

    public static void unjar(File dest, String jar) throws Exception {
        dest.mkdirs();
        JarFile jf = new JarFile(jar);
        try {
            Enumeration es = jf.entries();
            while(es.hasMoreElements()) {
                JarEntry je = (JarEntry)es.nextElement();
                String n = je.getName();
                File f = new File(dest, n);
                if (je.isDirectory()) {
                    f.mkdirs();
                } else {
                    if (f.exists()) {
                        f.delete();
                    } else {
                        f.getParentFile().mkdirs();
                    }
                    InputStream is = jf.getInputStream(je);
                    FileOutputStream os = new FileOutputStream(f);
                    try {
                        copyStream(is, os);
                    } finally {
                        os.close();
                    }
                }
                long time = je.getTime();
                if (time != -1) f.setLastModified(time);
            }
        } finally {
            jf.close();
        }
    }

    public static void unjar(File dest, InputStream is) throws Exception {
        dest.mkdirs();
        JarInputStream jis = new JarInputStream(is);
        try {
            while(true) {
                JarEntry je = jis.getNextJarEntry();
                if (je == null) break;
                String n = je.getName();
                File f = new File(dest, n);
                if (je.isDirectory()) {
                    f.mkdirs();
                } else {
                    if (f.exists()) {
                        f.delete();
                    } else {
                        f.getParentFile().mkdirs();
                    }
                    FileOutputStream os = new FileOutputStream(f);
                    try {
                        copyStream(jis, os);
                    } finally {
                        os.close();
                    }
                }
                long time = je.getTime();
                if (time != -1) f.setLastModified(time);
            }
        } finally {
            jis.close();
        }
    }

    public static void jar(File[] fs, String path) throws Exception {
        jar(fs, path, null);
    }

    public static void jar(File[] fs, String path, Manifest mf) throws Exception {
        File f = new File(path);
        FileOutputStream fos = new FileOutputStream(f);
        JarOutputStream jos = mf == null ? new JarOutputStream(fos) : new JarOutputStream(fos, mf);
        try {
            for (int i = 0; i < fs.length; i++) add(fs[i].getParentFile(), fs[i], jos);
        } finally {
            jos.close();
            fos.close();
        }
    }

    public static void add(File root, File f, JarOutputStream jos) throws Exception {
        int l = root.getAbsolutePath().length();
        String en = f.getAbsolutePath().substring(l + 1).replace('\\', '/');
        add(f, en, jos);
    }

    public static void add(File f, String name, JarOutputStream jos) throws Exception {
        String en = name;
        if(f.isDirectory()) en += "/";
        JarEntry entry = (en.endsWith("/")) ? null : new JarEntry(en);
        if(f.isDirectory()) {
            if("/".equals(en)) en = "";
            File[] fs = f.listFiles();
            if(fs != null) for (int i = 0; i < fs.length; i++)
              add(fs[i], en + fs[i].getName(), jos);
        } else {
            try {
                jos.putNextEntry(entry);
            } catch (Exception e) {
                return;
            }
            FileInputStream is = new FileInputStream(f);
            byte[] b = new byte[1024];
            int q = 0;
            while((q = is.available()) > 0) {
                if(q > 1024) q = 1024;
                q = is.read(b, 0, q);
                jos.write(b, 0, q);
            }
            is.close();
        }
        if(entry != null) jos.closeEntry();
    }

    public static void copy(InputStream f, OutputStream t) throws Exception {
        byte[] b = new byte[1024];
        int q = 0;
        while((q = f.read(b, 0, b.length)) > 0) t.write(b, 0, q);
        f.close();    
        t.close();
    }

    public static void unzip(File dest, String jar) throws Exception {
        dest.mkdirs();
        ZipFile zf = new ZipFile(jar);
        try {
            Enumeration es = zf.entries();
            while(es.hasMoreElements()) {
                ZipEntry je = (ZipEntry)es.nextElement();
                String n = je.getName();
                File f = new File(dest, n);
                if (je.isDirectory()) {
                    f.mkdirs();
                } else {
                    if (f.exists()) {
                        f.delete();
                    } else {
                        f.getParentFile().mkdirs();
                    }
                    InputStream is = zf.getInputStream(je);
                    FileOutputStream os = new FileOutputStream(f);
                    try {
                        copyStream(is, os);
                    } finally {
                        os.close();
                    }
                }
                long time = je.getTime();
                if (time != -1) f.setLastModified(time);
            }
        } finally {
            zf.close();
        }
    }

    public static String fileURLToFilePath(String url) {
        if(url == null) return null;
        url = url.replace('\\', '/');
///        if(!url.startsWith("file:/")) return url;
		if(!url.startsWith("file:")) return url;
        int iLast = url.lastIndexOf(':'), iFirst = url.indexOf(':');
        return (iLast == iFirst) ? url.substring(5) : url.substring(iLast - 1);
    }

    //// Relative path

	public static String getRelativePath(String rootpath, String path) {
		String[] r = tokenizePath(rootpath);
		String[] p = tokenizePath(path);
		if(r.length == 0 || p.length == 0 || !r[0].equalsIgnoreCase(p[0])) return null;
		int i = 0;
		while(i < r.length && i < p.length && r[i].equalsIgnoreCase(p[i])) ++i;
		StringBuffer sb = new StringBuffer();
		for (int k = i; k < r.length; k++) sb.append("/..");
		for (int k = i; k < p.length; k++) sb.append("/").append(p[k]);
		return sb.toString();
	}

	private static String[] tokenizePath(String s) {
		s = s.replace('\\', '/');
		StringTokenizer st = new StringTokenizer(s, "/");
		ArrayList l = new ArrayList();
		while(st.hasMoreTokens()) {
			String t = st.nextToken();
			if(t.length() == 0 || t.equals(".")) continue;
			if(t.equals("..")) {
				if(l.size() > 0) l.remove(l.size() - 1);
				continue;
			}
			l.add(t);
		}
		return (String[])l.toArray(new String[0]);
	}
	
	public static String encode(String text, String encoding) {
		if(true) return text;
		try {
			byte[] bs = text.getBytes(System.getProperty("file.encoding"));
			ByteArrayInputStream is = new ByteArrayInputStream(bs);
			InputStreamReader r = new InputStreamReader(is, encoding);
			char[] cs = new char[bs.length];
			int l = r.read(cs, 0, cs.length);
			return new String(cs, 0, l);
		} catch (Exception e) {
			if("UTF-8".equals(encoding)) return text;
			return encode(text, "UTF-8");
		}
	}

	public static String encodeDefault(String text) {
		return encode(text, System.getProperties().getProperty("file.encoding"));
	}
	
/*
	public static String decode(String text, String encoding) {
		if(true) return text;
		try {
			byte[] bs = text.getBytes(encoding);
			ByteArrayInputStream is = new ByteArrayInputStream(bs);
			InputStreamReader r = new InputStreamReader(is, System.getProperties().getProperty("file.encoding"));
			char[] cs = new char[bs.length];
			int l = r.read(cs, 0, cs.length);
			return new String(cs, 0, l);
		} catch (Exception e) {
			if("UTF-8".equals(encoding)) return text;
			return decode(text, "UTF-8");
		}
	}
	
	public static String decodeDefault(String text) {
		return decode(text, System.getProperties().getProperty("file.encoding"));
	}
*/

    public static boolean writeFile(File f, String value, String encoding) {
        try {
            try {
                if(f.isFile() && !isSameFile(f)) f.delete();
                if(!f.exists()) f.createNewFile();
            } catch (Exception e) {}
            FileOutputStream fs = new FileOutputStream(f);
            OutputStreamWriter osw = new OutputStreamWriter(fs, encoding);
            PrintWriter pw = new PrintWriter(osw);
            pw.print(value);
            pw.flush();
            pw.close();
            return true;
        } catch (Exception e) {
			//ignore
            return writeFileDefault(f, value);
        }
    }
    
    
    public static String getEncoding(String s) {
    	if(s == null) return null;
    	if(s.startsWith("<?xml")) {
    		int i = s.indexOf("encoding=");
    		if(i < 0) return null;
    		i += "encoding=".length() + 1;
    		int j = s.indexOf('\"', i);
    		if(j < 0) return null;
    		return s.substring(i, j);
    	}    	
    	return null;
    }
    
    static Set validEncodings = new HashSet();
    static Set invalidEncodings = new HashSet();
    
    public static String validateEncoding(String encoding, String defaultEncoding) {
    	if(encoding == null || encoding.equals(defaultEncoding)) return defaultEncoding;
    	if(validEncodings.contains(encoding)) return encoding;
    	if(invalidEncodings.contains(encoding)) return defaultEncoding;
    	try {
    		if(defaultEncoding.equals("UTF-16")) {
    			new String(XML_16, 0, XML_16.length, encoding);
    		} else {
    			new String(XML_8, 0, XML_8.length, encoding);
    		}
    		validEncodings.add(encoding);
    		return encoding;
    	} catch (Exception e) {
    		invalidEncodings.add(encoding);
    		return defaultEncoding;
    	}
    }
    
    static byte[] XML_8 = {(byte)'<',(byte)'?',(byte)'x',(byte)'m',(byte)'l'};
    static byte[] XML_16 = {(byte)-2,(byte)-1,(byte)0,(byte)60,(byte)0,(byte)63,(byte)0,(byte)120,(byte)0,(byte)109,(byte)0,(byte)108};

    public static String getEncoding(byte[] bs) {
    	if(bs.length < 20) return null;
    	if(startsWith(bs, XML_8)) {
    		int i = getIndex(bs, (byte)'?', 5);
    		if(i < 0) return "UTF-8";
    		String encoding = getEncoding(new String(bs, 0, i));
    		return validateEncoding(encoding, "UTF-8");
    	} else if(startsWith(bs, XML_16)) {
    		int i = getIndex(bs, (byte)'?', XML_16.length);
    		if(i < 0) return "UTF-16";
    		try {
    			String encoding = getEncoding(new String(bs, 0, i, "UTF-16"));
        		return validateEncoding(encoding, "UTF-16");
    		} catch (Exception e) {
    			return null;
    		}
    	}
    	return null;
    }
    
    static boolean startsWith(byte[] bs, byte[] prefix) {
    	for (int i = 0; i < prefix.length; i++) {
    		if(bs[i] != prefix[i]) return false;
    	}
    	return true;
    }
    
    static int getIndex(byte[] bs, byte b, int offset) {
    	for (int i = offset; i < bs.length; i++) {
    		if(bs[i] == b) return i;
    	}
    	return -1;
    }

}
