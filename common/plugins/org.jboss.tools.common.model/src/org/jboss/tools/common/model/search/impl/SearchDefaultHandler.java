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

import java.util.*;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.util.*;
import org.jboss.tools.common.meta.action.*;
import org.jboss.tools.common.meta.action.impl.*;
import org.jboss.tools.common.meta.action.impl.handlers.*;
//TODO check if this class is still needed
public class SearchDefaultHandler extends AbstractHandler {
    static String CONSTRAINT_ENTITY = "SearchValue";
    static SpecialWizard wizard = null;

    static SpecialWizard wizard() {
        return wizard;
    }

    public SearchDefaultHandler() {}

    public boolean isEnabled(XModelObject object) {
        return (wizard() != null && object != null || object.getPath() != null);
    }

    public void executeHandler(XModelObject object, Properties p) throws Exception {
        executeHandler(object, new XModelObject[]{object}, p);
    }

    public void executeHandler(XModelObject object, XModelObject[] objects, Properties p) throws Exception {
        if(!isEnabled(object) || objects == null || objects.length == 0) return;
        XModelObject sc = getModifiedSearch(object, objects);
        if(sc == null) return;
        wizard.setObject(new Object[]{sc});
        wizard.execute();
    }

    private XModelObject getModifiedSearch(XModelObject object, XModelObject[] objects) throws Exception {
        XModel model = object.getModel();
        XModelObject sc = findOrCreateDefaultSearch(object, objects);
        ServiceDialog d = model.getService();
        XEntityData ed = getSearchEntityData(sc);
        int i = d.showDialog("Search", getDialogTitle(objects), new String[]{"Ok", "Cancel"}, ed, ServiceDialog.QUESTION);
        if(i != 0) return null;
        Properties p = DefaultCreateHandler.extractProperties(ed);
        Enumeration en = p.keys();
        XModelObject vc = sc.getChildByPath("Match/value constraint");
        while(en.hasMoreElements()) {
            String n = (String)en.nextElement();
            String v = p.getProperty(n);
            model.changeObjectAttribute(vc, n, v);
        }
        model.saveOptions();
        return sc;
    }

    private XModelObject findOrCreateDefaultSearch(XModelObject object, XModelObject[] objects) {
        XModel model = object.getModel();
        XModelObject sr = model.getByPath("XStudio/Search");
        XModelObject ds = sr.getChildByPath("Default");
        Properties p = new Properties();
        StringBuffer sb = new StringBuffer();
        for (int i = 0; i < objects.length; i++) {
            if(i > 0) sb.append(";");
            sb.append(objects[i].getPath());
        }
        if(ds == null) {
            p.setProperty("name", "Default");
            p.setProperty("root", sb.toString());
            ds = XModelObjectLoaderUtil.createValidObject(model, "SearchCommand", p);
            sr.addChild(ds);
            sr.setModified(true);
        } else {
            model.changeObjectAttribute(ds, "root", sb.toString());
        }
        XModelObject match = ds.getChildByPath("Match");
        XModelObject v = null;
        if(match.getChildren(CONSTRAINT_ENTITY).length > 0) {
            v = match.getChildren(CONSTRAINT_ENTITY)[0];
        } else {
            p.clear();
            p.setProperty("name", "value constraint");
            v = XModelObjectLoaderUtil.createValidObject(model, CONSTRAINT_ENTITY, p);
            match.addChild(v);
            match.setModified(true);
        }
        return ds;
    }

    private String getDialogTitle(XModelObject[] os) {
        if(os.length == 1) return "Find in path " + os[0].getPath();
        StringBuffer sb = new StringBuffer("Find in paths:");
        for (int i = 0; i < os.length; i++) sb.append("\n").append(os[i].getPath());
        return sb.toString();
    }

    private XEntityData getSearchEntityData(XModelObject sc) {
        XModelObject c = sc.getChildByPath("Match/value constraint");
        String[][] ds = new String[][]{{"SearchValue", "yes"},
                                       {"text to find", "yes"},
                                       {"property name", "no"},
                                       {"ignore case", "no"}};
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

