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

import org.jboss.tools.common.model.XModelObjectConstants;
import org.jboss.tools.common.model.filesystems.FilePathHelper;

public class FileSystemPeer {
    private Hashtable<String,Info> p = new Hashtable<String,Info>();

    public FileSystemPeer() {}

    public void clear() {
        p.clear();
    }

    public void register(File f) {
        p.put(toKey(f), new Info(f));
    }

    public void unregister(File f) {
        p.remove(toKey(f));
    }

    public void unregisterDir(File f) {
        p.remove(toKey(f, true));
    }

    public boolean contains(File f) {
        return p.containsKey(toKey(f));
    }

    public boolean containsDir(File f) {
        return p.containsKey(toKey(f, true));
    }

    public boolean isUpdated(File f) {
        Info o = p.get(toKey(f));
        return (o == null) ? f.exists() : o.changed(f);
    }

    private String toKey(File f) {
        return toKey(f, f.isDirectory());
    }

    private String toKey(File f, boolean asDir) {
        String s = f.getAbsolutePath().replace('\\', '/');
        s = FilePathHelper.toPathPath(s);
        return (asDir) ? s + XModelObjectConstants.SEPARATOR : s;
    }

    class Info {
    	long lastModified;
    	long length;
    	Info(File f) {
    		lastModified = !f.exists() ? 0 : f.lastModified();
    		length = f.isFile() && f.exists() ? f.length() : 0;
    	}
    
    	public boolean equals(Info other) {
    		return lastModified != other.lastModified || length != other.length;
    	}
    
    	public boolean changed(File f) {
    		return equals(new Info(f));
    	}
    }

}
