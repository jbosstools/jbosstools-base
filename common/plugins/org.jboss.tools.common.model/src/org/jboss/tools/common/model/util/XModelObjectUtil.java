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
package org.jboss.tools.common.model.util;

import java.util.*;

import org.jboss.tools.common.meta.XAttribute;
import org.jboss.tools.common.meta.XChild;
import org.jboss.tools.common.meta.XModelEntity;
import org.jboss.tools.common.model.*;

import javax.naming.*;

public final class XModelObjectUtil {

    /*
     *  Creates unused name for child if pathpart equals name
     */
    public static String createNewChildName(String name, XModelObject parent) {
        if(parent.getChildByPath(name) == null) return name;
        int i = 0;
        while(parent.getChildByPath(name + (++i)) != null);
        return name + i;
    }

    public static String[] asStringArray(String s) {
        return asStringArray(s, ",;"); //$NON-NLS-1$
    }

    public static String[] asStringArray(String s, String separator) {
        if(s == null || s.length() == 0) return new String[0];
        StringTokenizer st = new StringTokenizer(s, separator);
        String[] result = new String[st.countTokens()];
        for (int i = 0; i < result.length; i++) result[i] = st.nextToken().trim();
        return result;
    }

	public static int[] asIntArray(String value) {
		String[] s = asStringArray(value);
		int[] result = new int[s.length];
		for (int i = 0; i < s.length; i++) {
			try {
				result[i] = Integer.parseInt(s[i]);
			} catch (NumberFormatException e) {
				result[i] = 0;
			}
		}
		return result;
	}

    public static String[][] asParameters(String s) {
        String[] sx = asStringArray(s);
        String[][] res = new String[sx.length][];
        for (int i = 0; i < sx.length; i++) {
            int d = sx[i].indexOf(' ');
            res[i] = (d < 0) ? new String[]{"", sx[i]} //$NON-NLS-1$
                     : new String[]{sx[i].substring(0, d), sx[i].substring(d + 1).trim()};
        }
        return res;
    }

    public static String getExpandedValue(XModelObject object, String attr, Context context) {
        return expand(object.getAttributeValue(attr), object.getModel(), context, new Properties());
    }

    public static String expand(String s, XModel model, Context context) {
        return expand(s, model, context, new Properties());
    }

    public static String expand(String s, XModel model, Context context, Properties found) {
        if(s == null || s.indexOf('%') < 0) return s;
        StringBuffer sb = new StringBuffer();
        expand(s, model, context, found, sb);
        return sb.toString();
    }

    private static void expand(String s, XModel model, Context context, Properties found, StringBuffer result) {
        if(s.length() == 0) return;
        int i = s.indexOf('%'), j = s.indexOf('%', i + 1);
        if(j <= i) {
            result.append(s);
        } else {
            result.append(s.substring(0, i));
            String q = s.substring(i + 1, j);
            s = s.substring(j + 1);
            String v = found.getProperty(q);
            if(v == null) {
                v = (model == null) ? null : model.getProperties().getProperty(q);
                if(v == null && model != null) {
                    int d = q.lastIndexOf('.');
                    String op = (d < 0) ? q : q.substring(0, d);
                    String oa = (d < 0) ? "value" : q.substring(d + 1); //$NON-NLS-1$
                    XModelObject o = model.getByPath(op);
                    v = (o == null) ? null : o.getAttributeValue(oa);
                }
                v = (v == null) ? "" : expand(v, model, context, found); //$NON-NLS-1$
                found.setProperty(q, v);
            }
            result.append(v);
            expand(s, model, context, found, result);
        }
    }

    public static Properties toProperties(XModelObject fs) {
        return toProperties(fs.getAttributeValue("info")); //$NON-NLS-1$
    }

    public static Properties toProperties(String s) {
        Properties props = new Properties();
        StringTokenizer st = new StringTokenizer(s, ","); //$NON-NLS-1$
        while (st.hasMoreElements()) {
            String t = st.nextToken();
            int i = t.indexOf('=');
            if (i < 0) {
                props.setProperty(t, ""); //$NON-NLS-1$
            } else {
                String n = t.substring(0, i).trim();
                String v = t.substring(i + 1).trim();
                props.setProperty(n, v);
            }
        }
        return props;
    }

    public static String toString(Properties props) {
        Iterator it = props.entrySet().iterator();
        String s = ""; //$NON-NLS-1$
        while (it.hasNext()) {
            Map.Entry entry = (Map.Entry)it.next();
            s += (s.length() > 0 ? "," : "") + entry.getKey() + "=" + entry.getValue(); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
        }
        return s;
    }
    
    public static String getDocumentPublicID(XModelObject object) {
    	XModelObject f = object;
    	while(f != null && f.getFileType() != XModelObject.FILE) f = f.getParent();
    	if(f == null) return null;
    	String r = f.getAttributeValue("publicId"); //$NON-NLS-1$
    	if(r != null) return r;
    	r = f.getAttributeValue("xsi:schemaLocation"); //$NON-NLS-1$
    	return r;
    }
    
    public static String getXMLLocalPath(XAttribute attribute) {
    	if(attribute == null) return null;
    	String tag = attribute.getModelEntity().getXMLSubPath();
    	if(tag == null || tag.length() == 0) tag = attribute.getModelEntity().getName();
    	String attr = attribute.getXMLName();
    	if(attr == null || attr.length() == 0) attr = attribute.getName();
    	int t = attr.indexOf("#text"); //$NON-NLS-1$
    	if(t >= 0) attr = attr.substring(0, t) + "text()[1]"; //$NON-NLS-1$
    	String path = tag.replace('.', '/') + '/' + attr.replace('.', '/');
    	while(path.lastIndexOf('/') != path.indexOf('/')) path = path.substring(path.indexOf('/') + 1);
    	if (t<0) path = path.replace('/','@');
    	return path;
    }


    private static Map<String, String> versionedChildEntities = new HashMap<String, String>();

    /**
     * Returns child entity name of parent entity which is equal to entityRoot or differs from it
     * by digital suffix.
     * @param parent
     * @param entityRoot
     * @return
     */
	public static String getVersionedChildEntity(XModelEntity parent, String entityRoot) {
		String key = parent.getName() + ":" + entityRoot; //$NON-NLS-1$
		if(versionedChildEntities.containsKey(key)) {
			return versionedChildEntities.get(key);
		}
		XChild[] cs = parent.getChildren();
		for (int i = 0; i < cs.length; i++) {
			String n = cs[i].getName();
			if(n.equals(entityRoot)) {
				versionedChildEntities.put(key, n);
				return n;
			}
			if(!n.startsWith(entityRoot)) continue;
			String suff = n.substring(entityRoot.length());
			if(Character.isDigit(suff.charAt(0))) {
				versionedChildEntities.put(key, n);
				return n;
			}
		}
		String result = "Unknown_Child_" + entityRoot + "_in_" + parent.getName(); //$NON-NLS-1$ //$NON-NLS-2$
		versionedChildEntities.put(key, result);
		return result;
	}    
    
}

