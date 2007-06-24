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
package org.jboss.tools.common.model.impl;

import org.jboss.tools.common.model.filesystems.FileSystem;
import org.jboss.tools.common.model.filesystems.FileSystemsHelper;

import java.io.*;
import java.util.*;
import java.net.*;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.jboss.tools.common.model.util.*;

public class XModelClassLoader extends URLClassLoader {
    private XModel model = null;
    private HashSet<String> urls = new HashSet<String>(), tempurls = new HashSet<String>();
    private HashSet<String> classes = new HashSet<String>();
    private boolean valid = true;

    public XModelClassLoader(XModel model) {
        super(new URL[0], ClassLoaderUtil.getClassLoader());
        this.model = model;
    }

    protected Class findClass(final String name) throws ClassNotFoundException {
        Class c = super.findClass(name);
        if(c != null) classes.add(name);
        return c;
    }

    public void validate() {
        XModelObject fs = FileSystemsHelper.getFileSystems(model);
        if(fs == null) return;
        XModelObject[] os = fs.getChildren();
        for (int i = 0; i < os.length; i++) {
            String loc = os[i].get("LOCATION");
            if(loc == null || loc.length() == 0) continue;
            loc = XModelObjectUtil.expand(loc, model, null);
            File f = new File(loc);
            try {
                loc = f.getCanonicalPath();
            } catch (Exception e) {
            	//ignore
            }
            if(loc == null) continue;
            String l = loc.toLowerCase();
            if(urls.contains(l)) continue;
            urls.add(l);
            try {
                FileSystem fsi = (FileSystem)os[i];
                String tl = XModelObjectUtil.expand(fsi.getTempLocation(), model, null);
                f = new File(tl).getCanonicalFile();
                addURL(f.toURL());
                tempurls.add(f.getAbsolutePath());
            } catch (Exception e) {
                ModelPlugin.log(e);
                tempurls.add(l);
            }
        }
    }

    public boolean isUsed() {
        return urls.size() > 0;
    }

    public String getClassPath() {
        StringBuffer sb = new StringBuffer();
        String[] s = new String[0]; 
        ///ClassLoaderUtil.getClassLoader().getClasspath();
        for(int i = 0; i < s.length; i++) sb.append(s[i]).append(File.pathSeparator);
        URL[] us = getURLs();
        for (int i = 0; i < us.length; i++) {
            String p = us[i].getPath();
            if((p.charAt(0)=='/' && p.charAt(2)==':') ||
              (p.charAt(0)=='\\' && p.charAt(2)==':')) {
                p = p.substring(1);
            }
            sb.append(p).append(File.pathSeparator);
        }
        return sb.toString();
    }

    public String getSourcePath() {
        Iterator i = tempurls.iterator();
        StringBuffer sb = new StringBuffer();
        while(i.hasNext()) {
            sb.append(i.next()).append(File.pathSeparator);
        }
        return sb.toString();
    }

    public boolean isLoaded(String name) {
        return classes.contains(name);
    }

    public void invalidate() {
        valid = false;
    }

    public boolean isValid() {
        return valid;
    }

}

