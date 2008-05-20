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

import java.util.*;
import org.jboss.tools.common.model.*;

public class TLDToPaletteHelper {
    public static final String START_TEXT = "start text";
    public static final String END_TEXT = "end text";
    public static final String REFORMAT = "automatically reformat tag body";
    public static final String DESCRIPTION = "description";
    public static final String URI = URIConstants.LIBRARY_URI;
    public static final String DEFAULT_PREFIX = URIConstants.DEFAULT_PREFIX;
    public static final String ADD_TAGLIB = "add taglib";

    public TLDToPaletteHelper() {}

    public XModelObject createMacroByTag(XModelObject tag, XModel model) {
        Properties p = new Properties();
        String parentname = getTldName(tag.getParent());
        String prefix = (parentname.length() == 0) ? "" : parentname + ":";
        String shortname = tag.getAttributeValue("name");
        String name = prefix + shortname;
        String tagname = shortname; ///name;
		p.setProperty("name", shortname);
        boolean empty = "empty".equals(tag.getAttributeValue("bodycontent"));
        if(!empty) p.setProperty(END_TEXT, "</" + tagname + ">");
        p.setProperty(START_TEXT, getStartText(tag, empty, tagname));
        p.setProperty(DESCRIPTION, getTagDescription(tag, empty, name));
        if(!empty) p.setProperty(REFORMAT, "yes");
        return model.createModelObject("SharableMacroHTML", p);
    }

    public static String getTldName(XModelObject tld) {
    	if(tld == null) return "";
        String n = tld.getAttributeValue("shortname");
    	if(n == null) return "";
        if(n.length() == 0) {
            n = tld.getAttributeValue("name");
            int q = n.lastIndexOf('-');
            if(q >= 0) n = n.substring(q + 1);
        }
        int s = n.lastIndexOf(' ');
        if(s >= 0) n = n.substring(s + 1);
        return n.toLowerCase();
    }

    private String getStartText(XModelObject tag, boolean empty, String name) {
        StringBuffer sb = new StringBuffer();
        sb.append("<").append(name);
        XModelObject[] as = tag.getChildren();
        boolean found = false;
        for (int i = 0; i < as.length; i++) {
            if(!TLDUtil.isAttribute(as[i])) continue;
            String required = as[i].getAttributeValue("required");
            if(!"true".equals(required) && !"yes".equals(required)) continue;
            sb.append(' ').append(as[i].getAttributeValue("name")).append("=\"");
            if(!found) {
                sb.append('|');
                found = true;
            }
            sb.append('"');
        }
        if(empty) sb.append("/");
        sb.append(">");
        return sb.toString();
    }

    private String getTagDescription(XModelObject tag, boolean empty, String name) {
//        String info = TLDUtil.getTagDescription(tag);
        StringBuffer sb = new StringBuffer();
        sb.append("<b>Syntax:</b><br><code>");
        if (empty) sb.append("&lt;" + name + " /&gt;"); else sb.append("&lt;" + name + "&gt;</code><br><code>&lt;/" + name + "&gt;");
        sb.append("</code><br>");
        sb.append("<b>Attributes:</b><br><code>");
		int k = 0;
		 XModelObject[] as = tag.getChildren();
		 for (int i = 0; i < as.length; i++) {
			 if(!TLDUtil.isAttribute(as[i])) continue;
			 if(!isRequired(as[i])) continue;
			 sb.append("<b>").append(as[i].getAttributeValue("name")).append("</b>");
			 ++k;
			 if(k < as.length) sb.append(", ");
		 }
		 for (int i = 0; i < as.length; i++) {
			 if(isRequired(as[i])) continue;
			 sb.append(as[i].getAttributeValue("name"));
			 ++k;
			 if(k < as.length) sb.append(", ");
		}
        
        sb.append("</code>");

/*
        sb.append("<html>").append("\n ");
        sb.append("<table width=\"300\">").append("\n  ");
        if(info.length() > 0) {
            sb.append("<tr>").append("\n   ");
            sb.append("<td><i><b>").append(info).append("</b></i></td>").append("\n  ");
            sb.append("</tr>").append("\n  ");
        }
        sb.append("<tr>").append("\n   ");
        sb.append("<td>").append("\n    ");
        sb.append("<font color=\"OLIVE\"><strong>Syntax:</strong></font> <code><br>");
        if(empty) sb.append("&lt;" + name + " /&gt;"); else sb.append("&lt;" + name + "&gt;<br>&lt;/" + name + "&gt;");
        sb.append("</code>").append("\n   ");
        sb.append("<br>").append("\n   ");
        sb.append("<font color=\"OLIVE\"><strong>Atributes:</strong></font>").append("\n   ");
        sb.append("<code><br>").append("\n    ");
        int k = 0;
        XModelObject[] as = tag.getChildren();
        for (int i = 0; i < as.length; i++) {
            if(!TLDUtil.isAttribute(as[i])) continue;
            if(!isRequired(as[i])) continue;
            sb.append("<b>").append(as[i].getAttributeValue("name")).append("</b>");
            ++k;
            if(k < as.length) sb.append(',');
            sb.append("\n    ");
        }
        for (int i = 0; i < as.length; i++) {
            if(isRequired(as[i])) continue;
            sb.append(as[i].getAttributeValue("name"));
            ++k;
            if(k < as.length) sb.append(',');
            sb.append("\n    ");
        }
        sb.append("</code>").append("\n   ");
        sb.append("</td>").append("\n  ");
        sb.append("</tr>").append("\n ");
        sb.append("</table>").append('\n');
        sb.append("</html>").append('\n');
*/
		
        return sb.toString();///XModelObjectLoaderUtil.saveToXMLAttribute(sb.toString());
    }

    private boolean isRequired(XModelObject attr) {
        String required = attr.getAttributeValue("required");
        return ("true".equals(required) || "yes".equals(required));
    }

    public XModelObject createTabByTLD(XModelObject tld, XModel model) {
    	return createGroupByTLD(tld, model, "SharablePageTabHTML");
    }

    public XModelObject createGroupByTLD(XModelObject tld, XModel model) {
    	return createGroupByTLD(tld, model, "SharableGroupHTML");
    }

    private XModelObject createGroupByTLD(XModelObject tld, XModel model, String entity) {
        Properties p = new Properties();
        p.setProperty("name", capitalize(getTldName(tld)));
        p.setProperty(DESCRIPTION, TLDUtil.getTagDescription(tld));
        p.setProperty(DEFAULT_PREFIX, getTldName(tld));
        p.setProperty(URIConstants.LIBRARY_URI, "" + tld.getAttributeValue("uri"));
        XModelObject tab = model.createModelObject(entity, p);
        XModelObject[] tags = tld.getChildren();
        for (int i = 0; i < tags.length; i++)
          if(TLDUtil.isTag(tags[i])) tab.addChild(createMacroByTag(tags[i], model));
        return tab;
    }


    private String capitalize(String s) {
        return (s.length() == 0) ? s : Character.toUpperCase(s.charAt(0)) + s.substring(1);
    }

}

