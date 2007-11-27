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

import org.jboss.tools.common.meta.*;
import org.jboss.tools.common.meta.action.*;
import org.jboss.tools.common.meta.constraint.impl.XAttributeConstraintAList;

public class HUtil {

    public HUtil() {}

    public static XAttributeData find(Object data, int index, String nm) {
        XAttributeData[] ad = find(data, index);
        return (ad == null) ? null : find(ad, nm);
    }

    public static XAttributeData[] find(Object data, int index) {
        XEntityData[] ed = (XEntityData[])data;
        return (index >= ed.length) ? null : ed[index].getAttributeData();
    }

    public static XAttributeData find(XAttributeData[] ad, String nm) {
        for (int i = 0; i < ad.length; i++) {
            XAttribute a = ad[i].getAttribute();
            if(a != null && a.getName().equals(nm)) return ad[i];
        }
        return null;
    }

    public static String getValue(XEntityData[] data, int index, String nm) {
        XAttributeData d = find(data, index, nm);
        String v = (d == null) ? null : d.getValue();
        return (v == null) ? null : d.getAttribute().isTrimmable()
                           ? v.trim() : v;
    }

    public static void hackAttributeConstraintList(XEntityData[] data, int index, String attr, String[] values) {
        XAttributeData ad = HUtil.find(data, index, attr);
        XAttribute a = ad.getAttribute();
        XAttributeConstraintAList c = (XAttributeConstraintAList)a.getConstraint();
        c.setValues(values);
    }

}
