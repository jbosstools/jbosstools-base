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
package org.jboss.tools.common.model.search.impl;

import java.text.MessageFormat;
import java.util.*;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.plugin.ModelMessages;
import org.jboss.tools.common.model.util.*;
import org.jboss.tools.common.meta.action.*;
import org.jboss.tools.common.meta.action.impl.*;
import org.jboss.tools.common.meta.action.impl.handlers.*;
//TODO check if this class is still needed
public class SearchDefaultHandler extends AbstractHandler {
    static String CONSTRAINT_ENTITY = "SearchValue"; //$NON-NLS-1$
    static SpecialWizard wizard = null;

    static SpecialWizard wizard() {
        return wizard;
    }

    public SearchDefaultHandler() {}

    public boolean isEnabled(XModelObject object) {
        return (wizard() != null && object != null || object.getPath() != null);
    }

    public void executeHandler(XModelObject object, Properties p) throws XModelException {
        executeHandler(object, new XModelObject[]{object}, p);
    }

    public void executeHandler(XModelObject object, XModelObject[] objects, Properties p) throws XModelException {
        if(!isEnabled(object) || objects == null || objects.length == 0) return;
        XModelObject sc = getModifiedSearch(object, objects);
        if(sc == null) return;
        wizard.setObject(new Object[]{sc});
        wizard.execute();
    }

    private XModelObject getModifiedSearch(XModelObject object, XModelObject[] objects) throws XModelException {
        XModel model = object.getModel();
        XModelObject sc = findOrCreateDefaultSearch(object, objects);
        ServiceDialog d = model.getService();
        XEntityData ed = getSearchEntityData(sc);
        int i = d.showDialog("Search", getDialogTitle(objects), new String[]{ModelMessages.OK, ModelMessages.Cancel}, ed, ServiceDialog.QUESTION);
        if(i != 0) return null;
        Properties p = DefaultCreateHandler.extractProperties(ed);
        Enumeration en = p.keys();
        XModelObject vc = sc.getChildByPath("Match/value constraint"); //$NON-NLS-1$
        while(en.hasMoreElements()) {
            String n = (String)en.nextElement();
            String v = p.getProperty(n);
            model.changeObjectAttribute(vc, n, v);
        }
        model.saveOptions();
        return sc;
    }

    private XModelObject findOrCreateDefaultSearch(XModelObject object, XModelObject[] objects) throws XModelException {
        XModel model = object.getModel();
        XModelObject sr = model.getByPath("XStudio/Search"); //$NON-NLS-1$
        XModelObject ds = sr.getChildByPath("Default"); //$NON-NLS-1$
        Properties p = new Properties();
        StringBuffer sb = new StringBuffer();
        for (int i = 0; i < objects.length; i++) {
            if(i > 0) sb.append(";"); //$NON-NLS-1$
            sb.append(objects[i].getPath());
        }
        if(ds == null) {
            p.setProperty(XModelObjectConstants.ATTR_NAME, "Default"); //$NON-NLS-1$
            p.setProperty("root", sb.toString()); //$NON-NLS-1$
            ds = XModelObjectLoaderUtil.createValidObject(model, "SearchCommand", p); //$NON-NLS-1$
            sr.addChild(ds);
            sr.setModified(true);
        } else {
            model.changeObjectAttribute(ds, "root", sb.toString()); //$NON-NLS-1$
        }
        XModelObject match = ds.getChildByPath("Match"); //$NON-NLS-1$
        XModelObject v = null;
        if(match.getChildren(CONSTRAINT_ENTITY).length > 0) {
            v = match.getChildren(CONSTRAINT_ENTITY)[0];
        } else {
            p.clear();
            p.setProperty(XModelObjectConstants.ATTR_NAME, "value constraint"); //$NON-NLS-1$
            v = XModelObjectLoaderUtil.createValidObject(model, CONSTRAINT_ENTITY, p);
            match.addChild(v);
            match.setModified(true);
        }
        return ds;
    }

    private String getDialogTitle(XModelObject[] os) {
        if(os.length == 1) return MessageFormat.format("Find in path {0}", os[0].getPath());
        StringBuffer sb = new StringBuffer("Find in paths:");
        for (int i = 0; i < os.length; i++) sb.append("\n").append(os[i].getPath()); //$NON-NLS-1$
        return sb.toString();
    }

    private XEntityData getSearchEntityData(XModelObject sc) {
        XModelObject c = sc.getChildByPath("Match/value constraint"); //$NON-NLS-1$
        String[][] ds = new String[][]{{"SearchValue", XModelObjectConstants.YES}, //$NON-NLS-1$
                                       {"text to find", XModelObjectConstants.YES}, //$NON-NLS-1$
                                       {"property name", XModelObjectConstants.NO}, //$NON-NLS-1$
                                       {"ignore case", XModelObjectConstants.NO}}; //$NON-NLS-1$
        XEntityData d = XEntityDataImpl.create(ds);
        XAttributeData[] as = d.getAttributeData();
        for (int i = 0; i < as.length; i++) {
            String n = as[i].getAttribute().getName();
            String v = c.getAttributeValue(n);
            if(v != null) as[i].setValue(v);
        }
        return d;
    }

}

