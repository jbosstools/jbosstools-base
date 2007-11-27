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
import java.util.*;

public class FileSystemPeer {
    private Hashtable<String,Long> p = new Hashtable<String,Long>();

    public FileSystemPeer() {}

    public void clear() {
        p.clear();
    }

    public void register(File f) {
        p.put(toKey(f), toLastModified(f));
    }

    public void unregister(File f) {
        p.remove(toKey(f));
    }

    public void unregisterDir(File f) {
        p.remove(toKey(f, true));
    }

    public boolean contains(File f) {
        return p.get(toKey(f)) != null;
    }

    public boolean containsDir(File f) {
        return p.get(toKey(f, true)) != null;
    }

    public boolean isUpdated(File f) {
        Object o = p.get(toKey(f));
        if(o == null) return f.exists();
        return f.lastModified() != ((Long)o).longValue();
    }

    private String toKey(File f) {
        return toKey(f, f.isDirectory());
    }

    private String toKey(File f, boolean asDir) {
        String s = f.getAbsolutePath().replace('\\', '/').toLowerCase();
        return (asDir) ? s + "/" : s;
    }

    private Long toLastModified(File f) {
        return !f.exists() ? new Long(0) : new Long(f.lastModified());
    }

}
