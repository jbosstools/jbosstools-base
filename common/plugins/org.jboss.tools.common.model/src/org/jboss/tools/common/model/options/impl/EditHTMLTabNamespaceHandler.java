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
package org.jboss.tools.common.model.options.impl;

import java.util.*;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.meta.action.impl.*;
import org.jboss.tools.common.meta.action.impl.handlers.*;

public class EditHTMLTabNamespaceHandler extends AbstractHandler {

    public EditHTMLTabNamespaceHandler() {}

    public boolean isEnabled(XModelObject object) {
        if(object == null || !object.isObjectEditable()) return false;
        XModelObject[] l = getMacroList(object);
        String ns = (l.length == 0) ? null : getNamespace(l[0]);
        return ns != null;
    }

    public void executeHandler(XModelObject object, Properties p) throws Exception {
        if(!isEnabled(object)) return;
        XModelObject[] l = getMacroList(object);
        String ns = (l.length == 0) ? null : getNamespace(l[0]);
        p = DefaultCreateHandler.extractProperties(data[0]);
        String ns2 = p.getProperty("namespace");
        for (int i = 0; i < l.length; i++) replace(l[i], ns, ns2);
    }

    private XModelObject[] getMacroList(XModelObject tab) {
        List<XModelObject> l = new ArrayList<XModelObject>();
        getMacroList(tab, l);
        return l.toArray(new XModelObject[0]);
    }

    private void getMacroList(XModelObject object, List<XModelObject> l) {
        if("SharableMacroHTML".equals(object.getModelEntity().getName())) {
            l.add(object);
        } else {
            XModelObject[] cs = object.getChildren();
            for (int i = 0; i < cs.length; i++) getMacroList(cs[i], l);
        }
    }

    private String getNamespace(XModelObject macro) {
        String start = macro.getAttributeValue("start text");
        if(!start.startsWith("<")) return null;
        int i = start.indexOf(':');
        int j = start.indexOf('>');
        return (j < 0) ? null : (i < 0 || i > j) ? "" : start.substring(1, i);
    }

    public void setDefaultData(XModelObject object) {
        super.setDefaultData(object);
        XModelObject[] l = getMacroList(object);
        if(l.length == 0) return;
        String ns = getNamespace(l[0]);
        if(ns != null && ns.length() > 0) data[0].getAttributeData()[0].setValue(ns);
    }

    private void replace(XModelObject o, String ns1, String ns2) {
        if(ns1.length() > 0) ns1 += ":";
        if(ns2.length() > 0) ns2 += ":";
        String start = o.getAttributeValue("start text");
        start = replace(start, "<", ns1, ns2);
        o.getModel().changeObjectAttribute(o, "start text", start);
        String end = o.getAttributeValue("end text");
        end = replace(end, "</", ns1, ns2);
        o.getModel().changeObjectAttribute(o, "end text", end);
        String d = o.getAttributeValue("description");
        d = replace(d, "&lt;", ns1, ns2);
        d = replace(d, "<", ns1, ns2);
        d = replace(d, "&lt;/", ns1, ns2);
        d = replace(d, "</", ns1, ns2);
        o.getModel().changeObjectAttribute(o, "description", d);
    }

    private String replace(String text, String pref, String b1, String b2) {
        int i = text.indexOf(pref + b1);
        return (i < 0) ? text : text.substring(0, i + pref.length()) + b2 +
                                text.substring(i + pref.length() + b1.length());
    }

}
