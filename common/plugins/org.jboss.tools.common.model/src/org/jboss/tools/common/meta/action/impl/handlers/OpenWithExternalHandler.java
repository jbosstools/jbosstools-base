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
import java.net.MalformedURLException;
import java.text.MessageFormat;
import java.util.*;
import org.jboss.tools.common.meta.action.impl.*;
import org.jboss.tools.common.meta.key.WizardKeys;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.plugin.ModelMessages;
import org.jboss.tools.common.model.plugin.ModelPlugin;
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
        if(!b) s = "?"; //$NON-NLS-1$
        ((XActionImpl)action).setDisplayName(s);
        return b;
    }

    public void executeHandler(XModelObject object, Properties p) throws XModelException {
        if(!isEnabled(object)) return;
		String displayName = WizardKeys.getMenuItemDisplayName(action, object == null ? null : object.getModelEntity());
        if(!checkSave(displayName, object)) return;
        String f = getFileName(object);
        String ext = OpenWithHelper.getLogicalExtension(object, action);
        XModelObject editor = OpenWithHelper.getEditorObject(object.getModel(), ext);
        if(editor == null) throw new RuntimeException("External editor for file extension '" + ext + "' is not set."); //$NON-NLS-1$ //$NON-NLS-2$
        start(displayName, f, editor);
    }

    static boolean checkSave(String actionname, XModelObject object) throws XModelException {
        if(!object.isModified() || !object.isActive() || !(object.getParent() instanceof FolderImpl)) return true;
        ServiceDialog d = object.getModel().getService();
        String mes = MessageFormat
				.format(
						"{0} is modified.\nDo you want to save it on disk before launching the external program?",
						DefaultCreateHandler.title(object, true));
        int i = d.showDialog(actionname, mes, new String[]{ModelMessages.Yes, ModelMessages.No, ModelMessages.Cancel}, null, ServiceDialog.QUESTION);
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
        String[] es = XModelObjectUtil.asStringArray(o.getAttributeValue("extensions")); //$NON-NLS-1$
        for (int i = 0; i < es.length; i++) {
            if(!es[i].toLowerCase().startsWith(ext.toLowerCase() + ":")) continue; //$NON-NLS-1$
            return es[i].substring(ext.length() + 1);
        }
        return null;
    }

    public static void start(String actionname, String filename, XModelObject editor) throws XModelException {
        int i = filename.indexOf('#');
        String fn = (i < 0) ? filename : filename.substring(0, i);
        if(!new File(fn).isFile()) throw new IllegalArgumentException("Cannot find file " + fn + "."); //$NON-NLS-1$ //$NON-NLS-2$
        if(OpenWithHelper.validatePath(actionname, editor)) new OWEProcess(editor, filename).start();
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
        return "."; //$NON-NLS-1$
    }

    protected void write(String s) {
        o.getModel().getOut().print(s);
    }

    protected void buildCommandLine(ArrayList<String> l) {
        String program = o.getAttributeValue("path"); //$NON-NLS-1$
        l.add(program);
        if("Internet Browser".equals(o.getAttributeValue(XModelObjectConstants.ATTR_NAME)) && !isUrl) { //$NON-NLS-1$
            try {
                java.net.URL u = new File(file).toURL();
                file = u.getProtocol() + "://" + u.getFile(); //$NON-NLS-1$
            } catch (MalformedURLException e) {
            	ModelPlugin.getPluginLog().logError(e);
            }
        }
        l.add(file);
    }

}

