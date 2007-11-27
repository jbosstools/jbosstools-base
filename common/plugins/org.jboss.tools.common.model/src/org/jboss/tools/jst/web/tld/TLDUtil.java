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
package org.jboss.tools.jst.web.tld;

import org.jboss.tools.common.model.*;

public class TLDUtil {
    static String FILE_ENTITIES = ".FileTLD_PRO.FileTLD_1_2.FileTLD_2_0.FileTLD_2_1.";
    static String TAG_ENTITIES = ".TLDTag.TLDTag12.TLDTag21.";
    static String ATTR_ENTITIES = ".TLDAttribute.TLDAttribute12.";

    public static boolean isTaglib(XModelObject o) {
        return isOfEntity(o, FILE_ENTITIES);
    }

    public static boolean isTag(XModelObject o) {
        return isOfEntity(o, TAG_ENTITIES);
    }

    public static boolean isAttribute(XModelObject o) {
        return isOfEntity(o, ATTR_ENTITIES);
    }

    private static boolean isOfEntity(XModelObject o, String entities) {
        return entities.indexOf("." + o.getModelEntity().getName() + ".") >= 0;
    }

    public static String getTagDescription(XModelObject o) {
        String attr = (o.getModelEntity().getAttribute("info") != null) ? "info" : "description";
        String s = o.getAttributeValue(attr);
        s = (s == null) ? "" : s.trim();
        StringBuffer sb = new StringBuffer();
        for (int i = 0; i < s.length(); i++) {
            char c = s.charAt(i);
            if(c == '<') sb.append("&lt;");
            else if(c == '>') sb.append("&gt;");
            else sb.append(c);
        }
        return sb.toString();
    }
    
}

