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
package org.jboss.tools.common.meta.action.impl.handlers;

import java.io.File;
import java.util.*;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.meta.action.*;
import org.jboss.tools.common.meta.action.impl.XEntityDataImpl;
import org.jboss.tools.common.model.util.*;
import org.jboss.tools.common.model.filesystems.XFileObject;
import org.jboss.tools.common.model.filesystems.impl.FileAnyImpl;

public class OpenWithHelper {
    static String EDITORS = "%Options%/External Programs";

    static String getFileName(XModelObject object) {
        ArrayList<String> l = new ArrayList<String>();
        XModelObject o = object;
        while(o != null) {
            int i = o.getFileType();
            if(i == XFileObject.FILE) l.add(FileAnyImpl.toFileName(o));
            else if(i == XFileObject.FOLDER) l.add(o.get("NAME"));
            else {
                l.add(XModelObjectUtil.expand((String)o.get("LOCATION"), o.getModel(), null));
                break;
            }
            o = o.getParent();
        }
        StringBuffer sb = new StringBuffer();
        for (int i = l.size() - 1; i >= 0; i--) {
            if(sb.length() > 0) sb.append('/');
            sb.append(l.get(i));
        }
        return sb.toString();
    }

    static String getExtension(String f) {
        return f.substring(f.lastIndexOf('.') + 1);
    }

    static XModelObject getEditorObject(XModel model, String ext) {
        XModelObject o = model.getByPath(EDITORS);
        String[] es = XModelObjectUtil.asStringArray(o.getAttributeValue("extensions"));
        for (int i = 0; i < es.length; i++) {
            if(!es[i].toLowerCase().startsWith(ext.toLowerCase() + ":")) continue;
            return o.getChildByPath(es[i].substring(ext.length() + 1));
        }
        return null;
    }

    static String[] getEditorList(XModel model) {
        XModelObject o = model.getByPath(EDITORS);
        XModelObject[] os = o.getChildren();
        String[] res = new String[os.length];
        for (int i = 0; i < res.length; i++) res[i] = os[i].get("NAME");
        return res;
    }

    static boolean validatePath(String actionname, XModelObject o) {
        String[] paths = getEnvironmentPaths();
        XModel model = o.getModel();
        String en = o.getModelEntity().getName();
        XEntityData[] dt = new XEntityData[]{XEntityDataImpl.create(new String[][]{{en, "yes"}, {"path", "yes"}})};
        String path = o.getAttributeValue("path").replace('\\','/');
        XAttributeData ad = HUtil.find(dt, 0, "path");
        ad.setValue(path);
        ServiceDialog d = model.getService();
        while(true) {
            String b = ad.getValue();
            if(b != null && b.length() > 0)
            if(fileExists(b, paths)) {
                if(!b.equals(path)) {
                    model.changeObjectAttribute(o, "path", b);
                    model.saveOptions();
                }
                return true;
            }
            int i = d.showDialog(actionname, "Enter valid path for " + o.getPresentationString(),
                                 new String[]{"OK", "Cancel"}, dt[0], ServiceDialog.QUESTION);
            if(i != 0) return false;
        }
    }

    static boolean fileExists(String filename, String[] paths) {
        filename = filename.replace('\\', '/');
        if(paths == null || filename.indexOf('/') >= 0) return new File(filename).isFile();
        for (int i = 0; i < paths.length; i++) {
            String f = paths[i] + "/" + filename;
            if(new File(f).isFile()) return true;
        }
        return false;
    }

    static String[] getEnvironmentPaths() {
        try {
//            String jlp = System.getProperties().getProperty("java.library.path");
            String jlp = OSHelper.getProperty("PATH", "");
            StringTokenizer st = new StringTokenizer(jlp, File.pathSeparator);
            String[] ps = new String[st.countTokens()];
            for (int i = 0; i < ps.length; i++) ps[i] = st.nextToken();
            return ps;
        } catch (Exception t) {
            return null;
        }
    }

    public static String getLogicalExtension(XModelObject object, XAction action) {
        String extension = action.getProperty("extension");
        return (extension != null) ? extension : getExtension(getFileName(object));
    }

}

