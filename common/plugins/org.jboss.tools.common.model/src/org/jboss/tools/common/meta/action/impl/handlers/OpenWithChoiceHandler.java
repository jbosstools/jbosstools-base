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

import java.util.*;
import org.jboss.tools.common.meta.action.*;
import org.jboss.tools.common.meta.action.impl.*;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.util.*;
import org.jboss.tools.common.model.filesystems.XFileObject;

public class OpenWithChoiceHandler extends AbstractHandler {

    public OpenWithChoiceHandler() {}

    public boolean isEnabled(XModelObject object) {
        if(object == null || object.getFileType() != XFileObject.FILE) return false;
        return check(object);
    }

    public void executeHandler(XModelObject object, Properties p) throws Exception {
        if(!isEnabled(object)) return;
        if(!OpenWithExternalHandler.checkSave(action.getDisplayName(), object)) return;
        ServiceDialog d = object.getModel().getService();
        String ext = OpenWithHelper.getLogicalExtension(object, action);
        XAttributeData a1 = HUtil.find(data, 0, "name");
        XAttributeData a2 = HUtil.find(data, 0, "default");
        XModelObject o = OpenWithHelper.getEditorObject(object.getModel(), ext);
        String oldname = null;
        if(o != null) {
            oldname = o.get("NAME");
            a1.setValue(oldname);
            a2.setValue("yes");
        } else {
            a2.setValue("no");
        }
        int i = d.showDialog("Open With", "Select external program", new String[]{"Ok", "Cancel"}, data[0], ServiceDialog.QUESTION);
        if(i != 0) return;
        DefaultCreateHandler.extractProperties(data[0]);
        String en = HUtil.getValue(data, 0, "name");
        boolean def = "yes".equals(HUtil.getValue(data, 0, "default"));
        if(def && !en.equals(oldname)) {
            changeDefaultEditor(object.getModel(), ext, en);
        } else if(!def && en.equals(oldname)) {
            removeDefaultEditor(object.getModel(), ext);
        }
        XModelObject editor = object.getModel().getByPath(OpenWithHelper.EDITORS + "/" + en);
        String f = OpenWithHelper.getFileName(object);
        OpenWithExternalHandler.start(action.getDisplayName(), f, editor);
    }

    protected boolean check(XModelObject object) {
        String[] es = OpenWithHelper.getEditorList(object.getModel());
        HUtil.hackAttributeConstraintList(data, 0, "name", es);
        return es.length > 0;
    }

    static void changeDefaultEditor(XModel model, String ext, String editor) {
        XModelObject o = model.getByPath(OpenWithHelper.EDITORS);
        StringBuffer sb = new StringBuffer();
        String ov = o.getAttributeValue("extensions");
        String[] es = XModelObjectUtil.asStringArray(ov);
        boolean done = false;
        for (int i = 0; i < es.length; i++) {
            if(!es[i].toLowerCase().startsWith(ext.toLowerCase() + ":")) {
                append(sb, es[i]);
            } else {
                done = true;
                append(sb, ext + ":" + editor);
            }
        }
        if(!done) append(sb, ext + ":" + editor);
        applyChange(o, ov, sb.toString());
    }

    static void removeDefaultEditor(XModel model, String ext) {
        XModelObject o = model.getByPath(OpenWithHelper.EDITORS);
        StringBuffer sb = new StringBuffer();
        String ov = o.getAttributeValue("extensions");
        String[] es = XModelObjectUtil.asStringArray(ov);
        for (int i = 0; i < es.length; i++) {
            if(!es[i].toLowerCase().startsWith(ext.toLowerCase() + ":")) {
                append(sb, es[i]);
            }
        }
        applyChange(o, ov, sb.toString());
    }

    private static void append(StringBuffer sb, String item) {
        if(sb.length() > 0) sb.append(';');
        sb.append(item);
    }

    private static void applyChange(XModelObject editors, String ov, String nv) {
        if(nv.equals(ov)) return;
        editors.getModel().changeObjectAttribute(editors, "extensions", nv);
        editors.getModel().saveOptions();
    }

}
