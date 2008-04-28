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
package org.jboss.tools.common.model.util;

import java.io.*;
import java.util.jar.*;

/**
 * @deprecated This class has been replaced to org.jboss.tools.common plugin (org.jboss.tools.common.util.FileUtil)
 */
public final class FileUtil {

    public FileUtil() {}

    public static String readFile(File f) {
        return org.jboss.tools.common.util.FileUtil.readFile(f);
    }

    public static boolean isTextFile(File f, int length) {
    	return org.jboss.tools.common.util.FileUtil.isTextFile(f, length);
    }

    public static boolean isText(String body) {
    	return org.jboss.tools.common.util.FileUtil.isText(body);
    }

    public static String readStream(InputStream is) {
    	return org.jboss.tools.common.util.FileUtil.readStream(is);
    }

    public static boolean writeFile(File f, String value) {
    	return org.jboss.tools.common.util.FileUtil.writeFile(f, value);
    }

    public static boolean copyFile(File source, File dest, boolean mkdirs) {
    	return org.jboss.tools.common.util.FileUtil.copyFile(source, dest, mkdirs);
    }
    
    public static boolean copyFile(File source, File dest) {
    	return org.jboss.tools.common.util.FileUtil.copyFile(source, dest);
    }
    
    public static void copyStream(InputStream is, OutputStream os) throws IOException {
    	org.jboss.tools.common.util.FileUtil.copyStream(is, os);
    }
    
    public static void clear(File f) {
    	org.jboss.tools.common.util.FileUtil.clear(f);
    }

    public static void remove(File f) {
    	org.jboss.tools.common.util.FileUtil.remove(f);
    }

    static boolean isSameFile(File f) {
    	return org.jboss.tools.common.util.FileUtil.isSameFile(f);
    }

    public static void copyDir(File from, File to) {
    	org.jboss.tools.common.util.FileUtil.copyDir(from, to);
    }

    public static void copyDir(File from, File to, boolean mkdirs) {
        copyDir(from, to, mkdirs, true);
    }

    public static void copyDir(File from, File to, boolean mkdirs, boolean includeSubdirs) {
        org.jboss.tools.common.util.FileUtil.copyDir(from, to, mkdirs, includeSubdirs);
    }
    
    public static void copyDir(File from, boolean includeSubdirs, File to) {
        org.jboss.tools.common.util.FileUtil.copyDir(from, includeSubdirs, to);
    }

    public static void unjar(File dest, String jar) throws Exception {
        org.jboss.tools.common.util.FileUtil.unjar(dest, jar);
    }

    public static void unjar(File dest, InputStream is) throws Exception {
        org.jboss.tools.common.util.FileUtil.unjar(dest, is);
    }

    public static void jar(File[] fs, String path) throws Exception {
        org.jboss.tools.common.util.FileUtil.jar(fs, path);
    }
    
    public static void jar(File[] fs, String path, Manifest mf) throws Exception {
        org.jboss.tools.common.util.FileUtil.jar(fs, path, mf);
    }

    public static void add(File root, File f, JarOutputStream jos) throws Exception {
        org.jboss.tools.common.util.FileUtil.add(root, f, jos);
    }

    public static void add(File f, String name, JarOutputStream jos) throws Exception {
        org.jboss.tools.common.util.FileUtil.add(f, name, jos);
    }

    public static void copy(InputStream f, OutputStream t) throws Exception {
        org.jboss.tools.common.util.FileUtil.copy(f, t);
    }

    public static String fileURLToFilePath(String url) {
        return org.jboss.tools.common.util.FileUtil.fileURLToFilePath(url);
    }
    
	public static String getRelativePath(String rootpath, String path) {
        return org.jboss.tools.common.util.FileUtil.getRelativePath(rootpath, path);
	}

}
