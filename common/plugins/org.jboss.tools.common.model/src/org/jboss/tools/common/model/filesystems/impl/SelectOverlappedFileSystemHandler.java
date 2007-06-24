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

import org.jboss.tools.common.meta.action.XActionInvoker;
import org.jboss.tools.common.meta.action.impl.*;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.impl.*;
import org.jboss.tools.common.model.util.*;
import org.jboss.tools.common.model.util.FindObjectHelper;

public class SelectOverlappedFileSystemHandler extends AbstractHandler {

    public SelectOverlappedFileSystemHandler() {}

    public boolean isEnabled(XModelObject o) {
        return o != null && ("true".equals(o.get("overlapped")));
    }

    public void executeHandler(XModelObject object, java.util.Properties p) throws Exception {
        XModelObject fs = getOverlappedFileSystem(object);
        if(fs == null) {
        	if(object.isActive() && object.getFileType() == XModelObject.FILE
        			&& XActionInvoker.getAction("OpenFile", object) != null) {
       			XActionInvoker.invoke("OpenFile", object, null);
        	}
        	return;
        } 
        Properties fsp = XModelObjectUtil.toProperties(fs);
        if("yes".equals(fsp.getProperty("hidden", "no"))) {
            fsp.setProperty("hidden", "no");
            fs.setAttributeValue("info", XModelObjectUtil.toString(fsp));
            fs.setModified(true);
            XModelImpl m = (XModelImpl)object.getModel();
            m.fireStructureChanged(fs.getParent());
        }
        FindObjectHelper.findModelObject(fs, FindObjectHelper.EVERY_WHERE);
    }

    static XModelObject getOverlappedFileSystem(XModelObject source) {
        String fp = getAbsoluteFileFolderPath(source);
        if(fp == null) return null;
        XModelObject fs = source.getModel().getByPath("FileSystems");
        if(fs == null) return null;
        XModelObject[] cs = fs.getChildren();
        for (int i = 0; i < cs.length; i++)
          if(fp.equals(getAbsoluteFileSystemPath(cs[i]))) return cs[i];
        return null;
    }

    private static String getAbsoluteFileSystemPath(XModelObject fso) {
        String path = XModelObjectUtil.getExpandedValue(fso, "location", null);
        try {
            return new File(path).getCanonicalPath().replace('\\', '/').toLowerCase();
        } catch (Exception e) {
        	//ignore
            return null;
        }
    }

    private static String getAbsoluteFileFolderPath(XModelObject f) {
        String path = f.getPath();
        String rpath = XModelObjectLoaderUtil.getResourcePath(f);
		if(path == null || rpath == null) return null;
        XModelObject fso = f.getModel().getByPath(path.substring(0, path.length() - rpath.length()));
        return (getAbsoluteFileSystemPath(fso) + rpath).toLowerCase();
    }

}

