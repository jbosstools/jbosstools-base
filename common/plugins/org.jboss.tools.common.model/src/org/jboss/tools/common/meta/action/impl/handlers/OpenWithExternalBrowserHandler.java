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

import org.jboss.tools.common.model.*;

public class OpenWithExternalBrowserHandler extends OpenWithExternalHandler {

    public OpenWithExternalBrowserHandler() {}

    /*
    public boolean isEnabled(XModelObject object) {
        if(object == null || object.getFileType() != object.FILE) return false;
        String s = getEditorName(object);
        boolean b = (s != null);
        if(!b) s = "?";
        ((XActionImpl)action).setDisplayName(s);
        return b;
    }

    public void executeHandler(XModelObject object, Properties p) throws Exception {
        if(!isEnabled(object)) return;
        String f = getFileName(object);
        String ext = OpenWithHelper.getExtension(f);
        XModelObject editor = OpenWithHelper.getEditorObject(object.getModel(), ext);
        if(editor == null) throw new RuntimeException("External editor for file extension '" + ext + "' is not set.");
        start(action.getDisplayName(), f, editor);
    }

    protected String getFileName(XModelObject object) {
        return OpenWithHelper.getFileName(object);
    }
 */

//    private String getEditorName(XModelObject object) {
//        XModelObject o = object.getModel().getByPath(OpenWithHelper.EDITORS);
//        String[] es = XModelObjectUtil.asStringArray(o.getAttributeValue("extensions"));
//        for (int i = 0; i < es.length; i++) {
//            if(!es[i].toLowerCase().startsWith("html:")) continue;
//            return es[i].substring("html:".length());
//        }
//        return null;
//    }

    public static void start(String actionname, String url, XModelObject editor) throws Exception {
        if(OpenWithHelper.validatePath(actionname, editor)) new OWEProcess(editor, url).start();
    }
}
