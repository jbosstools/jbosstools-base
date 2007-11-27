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

import java.io.*;
import java.util.*;
import org.jboss.tools.common.meta.action.impl.*;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.util.*;
import org.jboss.tools.common.model.engines.impl.XProcess;
import org.jboss.tools.common.model.filesystems.XFileObject;
import org.jboss.tools.common.model.filesystems.impl.*;

public class OpenWithExternalHandler extends AbstractHandler {

    public OpenWithExternalHandler() {}

    public boolean isEnabled(XModelObject object) {
        if(object == null || object.getFileType() != XFileObject.FILE) return false;
        String s = getEditorName(object);
        boolean b = (s != null);
        if(!b) s = "?";
        ((XActionImpl)action).setDisplayName(s);
        return b;
    }

    public void executeHandler(XModelObject object, Properties p) throws Exception {
        if(!isEnabled(object)) return;
        if(!checkSave(action.getDisplayName(), object)) return;
        String f = getFileName(object);
        String ext = OpenWithHelper.getLogicalExtension(object, action);
        XModelObject editor = OpenWithHelper.getEditorObject(object.getModel(), ext);
        if(editor == null) throw new RuntimeException("External editor for file extension '" + ext + "' is not set.");
        start(action.getDisplayName(), f, editor);
    }

    static boolean checkSave(String actionname, XModelObject object) {
        if(!object.isModified() || !object.isActive() || !(object.getParent() instanceof FolderImpl)) return true;
        ServiceDialog d = object.getModel().getService();
        String mes = DefaultCreateHandler.title(object, true) + " is modified.\n" +
                     "Do you want to save it on disk before launching the external program?";
        int i = d.showDialog(actionname, mes, new String[]{"Yes", "No", "Cancel"}, null, ServiceDialog.QUESTION);
        if(i == 0) {
            ((FolderImpl)object.getParent()).saveChild(object);
            return !object.isModified();
        } else return (i == 1);
    }

    protected String getFileName(XModelObject object) {
        return OpenWithHelper.getFileName(object);
    }

    private String getEditorName(XModelObject object) {
        String ext = OpenWithHelper.getLogicalExtension(object, action);
        XModelObject o = object.getModel().getByPath(OpenWithHelper.EDITORS);
        String[] es = XModelObjectUtil.asStringArray(o.getAttributeValue("extensions"));
        for (int i = 0; i < es.length; i++) {
            if(!es[i].toLowerCase().startsWith(ext.toLowerCase() + ":")) continue;
            return es[i].substring(ext.length() + 1);
        }
        return null;
    }

    public static void start(String actionname, String filename, XModelObject editor) throws Exception {
        int i = filename.indexOf('#');
        String fn = (i < 0) ? filename : filename.substring(0, i);
        if(!new File(fn).isFile()) throw new RuntimeException("Cannot find file " + fn + ".");
        if(OpenWithHelper.validatePath(actionname, editor)) new OWEProcess(editor, filename).start();
    }

    public static void startExplorer(XModel model, String url) throws Exception {
        XModelObject editor = model.getByPath("%Options%/External Programs/Internet Browser");
        if(editor == null) throw new Exception("External Program 'Internet Browser' is not set in Options.");
        if(OpenWithHelper.validatePath("Open", editor)) new OWEProcess(editor, url, true).start();
    }

}

class OWEProcess extends XProcess {
    private XModelObject o;
    private String file;
    private boolean isUrl;

    public OWEProcess(XModelObject o, String file) {
        this(o, file, false);
    }

    public OWEProcess(XModelObject o, String file, boolean isUrl) {
        this.o = o;
        this.file = file;
        this.isUrl = isUrl;
    }

    protected String getRoot() {
        return ".";
    }

    protected void write(String s) {
        o.getModel().getOut().print(s);
    }

    protected void buildCommandLine(ArrayList<String> l) {
        String program = o.getAttributeValue("path");
        l.add(program);
        if("Internet Browser".equals(o.getAttributeValue("name")) && !isUrl) {
            try {
                java.net.URL u = new File(file).toURL();
                file = u.getProtocol() + "://" + u.getFile();
            } catch (Exception e) {
            	//ignore
            }
        }
        l.add(file);
    }

}

