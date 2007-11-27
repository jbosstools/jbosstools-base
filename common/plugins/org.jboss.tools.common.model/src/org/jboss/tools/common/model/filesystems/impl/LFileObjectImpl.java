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
package org.jboss.tools.common.model.filesystems.impl;

import java.io.*;
import org.jboss.tools.common.model.util.XModelObjectLoaderUtil;

public class LFileObjectImpl implements LFileObject {
    protected String root, aliaspath, relpath;
    private File f = null;

    public LFileObjectImpl(String root, String alias, String relpath) {
        this.root = root;
        this.relpath = relpath;
        aliaspath = relpath.length() == 0 ? alias : alias + '/' + relpath;
        String fn = (relpath.length() == 0) ? "" + root : "" + root + '/' + relpath;
        f = new File(fn);
    }

    public String getName() {
        return (f == null) ? null : f.getName();
    }

    public boolean exists() {
        return f != null && f.exists();
    }

    public boolean isDirectory() {
        return f != null && f.isDirectory();
    }

    public boolean isFile() {
        return f != null && f.isFile();
    }

    public long lastModified() {
        return (f == null) ? 0 : f.lastModified();
    }

    public String getPath() {
        return aliaspath;
    }

    public boolean canWrite() {
        return f != null && f.canWrite();
    }

    public String read() {
        return f == null ? "" : XModelObjectLoaderUtil.readFile(f);
    }

    public void write(String s) {
        XModelObjectLoaderUtil.writeFile(f, s);
    }

    public String[] listFiles() {
        File[] fs = (f == null) ? null : f.listFiles();
        if(fs == null) return new String[0];
        String[] rs = new String[fs.length];
        String rp = getPath();
        for (int i = 0; i < rs.length; i++) rs[i] = rp + '/' + fs[i].getName();
        return rs;
    }

    public boolean mkdirs() {
        return f != null && f.mkdirs();
    }

    public boolean delete() {
        if(f == null) return true;
        XModelObjectLoaderUtil.remove(f);
        return f.exists();
    }

}

