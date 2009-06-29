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
package org.jboss.tools.common.meta.impl.documentation;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.Iterator;

import org.jboss.tools.common.model.XModel;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.XModelObjectConstants;
import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.jboss.tools.common.model.util.XMLUtil;
import org.jboss.tools.common.model.util.XModelObjectLoaderUtil;
import org.w3c.dom.Element;

public class DocumentGenerator {
    private XModelObject meta = null;
    private String filename = null;
    private Parents parents = null;

    public DocumentGenerator() {}

    public void setModel(XModel model) {
        meta = model.getRoot().getChildByPath("MetaModel"); //$NON-NLS-1$
    }

    public void generate(String filename) {
        if(meta == null) throw new IllegalStateException("Meta root is not set."); //$NON-NLS-1$
        this.filename = filename;
        Element g = XMLUtil.createDocumentElement("html"); //$NON-NLS-1$
        generateTitle(g);
        generateHead(g);
        generateBody(g);
        try {
            XModelObjectLoaderUtil.serialize(g, filename);
            replace();
        } catch (IOException e) {
        	ModelPlugin.getPluginLog().logError(e);
        }
    }

    protected void generateTitle(Element e) {
        Element t = XMLUtil.createElement(e, "title"); //$NON-NLS-1$
        XMLUtil2.createText(t, "Entity list");
    }

    protected void generateHead(Element e) {
        Element h = XMLUtil.createElement(e, "head"); //$NON-NLS-1$
        Element s = XMLUtil.createElement(h, "style"); //$NON-NLS-1$
        XMLUtil2.createText(s, new StyleGenerator().getStyle());
    }

    protected void generateBody(Element e) {
        Element b = XMLUtil.createElement(e, XModelObjectConstants.ATTR_NAME_BODY);
        XModelObject entities = meta.getChildren("MetaEntities")[0]; //$NON-NLS-1$
        parents = new Parents();
        parents.set(entities);
        parents.compile();
        new ContentGenerator().generate(b, entities);
        processGroup(b, entities);
    }

    protected void processGroup(Element e, XModelObject o) {
        XModelObject[] es = o.getChildren("MetaEntity"); //$NON-NLS-1$
        for (int i = 0; i < es.length; i++) processEntity(e, es[i]);
        XModelObject[] gs = o.getChildren("MetaEntityGroup"); //$NON-NLS-1$
        for (int i = 0; i < gs.length; i++) processGroup(e, gs[i]);
    }

    protected void processEntity(Element e, XModelObject o) {
        XMLUtil2.hr(e);
        Element t = XMLUtil.createElement(e, "table"); //$NON-NLS-1$
        XMLUtil2.entityRow(t, "Name:", o.getAttributeValue(XModelObjectConstants.ATTR_NAME));
        XMLUtil2.simpleRow(t, "Editor:", o.getAttributeValue("editor")); //$NON-NLS-2$
        processHierarchy(e, o);
        processAttributeList(e, o.getChildren("MetaAttributes")[0]); //$NON-NLS-1$
        processActionList(e, o.getChildren("MetaActionList")[0]); //$NON-NLS-1$
    }

    protected void processHierarchy(Element e, XModelObject o) {
        Element t = XMLUtil.createElement(e, "table"); //$NON-NLS-1$
        t.setAttribute("width", "600"); //$NON-NLS-1$ //$NON-NLS-2$
        Element tr = XMLUtil.createElement(t, "tr"); //$NON-NLS-1$
        for (int i = 0; i < 2; i++) {
            Element td = XMLUtil.createElement(tr, "td"); //$NON-NLS-1$
            td.setAttribute("width", "300"); //$NON-NLS-1$ //$NON-NLS-2$
        }
        tr = XMLUtil.createElement(t, "tr"); //$NON-NLS-1$
        Element td = XMLUtil.createElement(tr, "td"); //$NON-NLS-1$
        td.setAttribute("valign", "top"); //$NON-NLS-1$ //$NON-NLS-2$
        processParents(td, o);
        td = XMLUtil.createElement(tr, "td"); //$NON-NLS-1$
        td.setAttribute("valign", "top"); //$NON-NLS-1$ //$NON-NLS-2$
        processChildren(td, o.getChildren("MetaChildren")[0]); //$NON-NLS-1$

    }

    protected void processParents(Element e, XModelObject o) {
        Element lt = XMLUtil2.createSubTitle(e, "Can be inside:");
        ArrayList<String> v = parents.get(o);
        if(v.size() == 0) {
            XMLUtil2.createText(lt, "None");
            XMLUtil.createElement(lt, "br"); //$NON-NLS-1$
        } else for (int i = 0; i < v.size(); i++) {
            Element s = XMLUtil.createElement(lt, "span"); //$NON-NLS-1$
            s.setAttribute("class", "value"); //$NON-NLS-1$ //$NON-NLS-2$
            XMLUtil2.createEntityReference(s, v.get(i));
            XMLUtil.createElement(lt, "br"); //$NON-NLS-1$
        }
    }

    protected void processChildren(Element e, XModelObject o) {
        Element lt = XMLUtil2.createSubTitle(e, "Possible children:");
        XModelObject[] as = o.getChildren();
        if(as.length == 0) {
            XMLUtil2.createText(lt, "None");
            return;
        }
        Element t = createTable(lt);
        Element tr = XMLUtil.createElement(t, "tr"); //$NON-NLS-1$
        createHeaderCell(tr, "Name", 0);
        createHeaderCell(tr, "Required", 0);
        for (int i = 0; i < as.length; i++) {
            tr = XMLUtil.createElement(tr, "tr"); //$NON-NLS-1$
            Element td = XMLUtil.createElement(tr, "td"); //$NON-NLS-1$
            td.setAttribute("class", "value"); //$NON-NLS-1$ //$NON-NLS-2$
            XMLUtil2.createEntityReference(td, as[i].getAttributeValue(XModelObjectConstants.ATTR_NAME));
            createValueCell(tr, as[i].getAttributeValue("required")); //$NON-NLS-1$
        }
    }

    protected void processAttributeList(Element e, XModelObject o) {
        Element lt = XMLUtil2.createSubTitle(e, "Attribute List:");
        Element t = createTable(lt);
        Element tr = XMLUtil.createElement(t, "tr"); //$NON-NLS-1$
        createHeaderCell(tr, "Name", 130);
        createHeaderCell(tr, "Visible", 50);
        createHeaderCell(tr, "Editor", 0);
        createHeaderCell(tr, "Default Value", 0);
        createHeaderCell(tr, "Possible Values", 0);
        XModelObject[] as = o.getChildren("MetaAttribute"); //$NON-NLS-1$
        for (int i = 0; i < as.length; i++) {
            tr = XMLUtil.createElement(tr, "tr"); //$NON-NLS-1$
            createValueCell(tr, as[i].getAttributeValue(XModelObjectConstants.ATTR_NAME));
            createValueCell(tr, as[i].getAttributeValue("visibility")); //$NON-NLS-1$
            XModelObject oe = as[i].getChildren("MetaAttributeEditor")[0]; //$NON-NLS-1$
            String ed = oe.getAttributeValue(XModelObjectConstants.ATTR_NAME);
            createValueCell(tr, ed);
            String dv = as[i].getAttributeValue("default value"); //$NON-NLS-1$
            createValueCell(tr, dv);
            XModelObject oc = as[i].getChildren("MetaAttributeConstraint")[0]; //$NON-NLS-1$
            String cl = oc.getAttributeValue("loader"); //$NON-NLS-1$
            if(cl.startsWith("List")) { //$NON-NLS-1$
                processListConstraint(tr, oc);
            } else if(cl.length() == 0 && "Uneditable".equals(ed)) { //$NON-NLS-1$
                createValueCell(tr, dv);
            } else if(cl.length() == 0 && "Int".equals(ed)) { //$NON-NLS-1$
                createValueCell(tr, "integer"); //$NON-NLS-1$
            } else {
                createValueCell(tr, "any"); //$NON-NLS-1$
            }
        }
    }

    Element createTable(Element e) {
        Element t = XMLUtil.createElement(e, "table"); //$NON-NLS-1$
        t.setAttribute("border", "1"); //$NON-NLS-1$ //$NON-NLS-2$
        t.setAttribute("cellspacing", "0"); //$NON-NLS-1$ //$NON-NLS-2$
        t.setAttribute("cellpadding", "2"); //$NON-NLS-1$ //$NON-NLS-2$
        return t;
    }
    void createHeaderCell(Element e, String name, int width) {
        Element td = XMLUtil.createElement(e, "td"); //$NON-NLS-1$
        td.setAttribute("class", XModelObjectConstants.ATTR_NAME); //$NON-NLS-1$
        if(width > 0) td.setAttribute("width", "" + width); //$NON-NLS-1$ //$NON-NLS-2$
        XMLUtil2.createText(td, name);
    }
    void createValueCell(Element e, String value) {
        Element td = XMLUtil.createElement(e, "td"); //$NON-NLS-1$
        if(value.length() == 0) value = "&nbsp;"; //$NON-NLS-1$
        td.setAttribute("class", "value"); //$NON-NLS-1$ //$NON-NLS-2$
        XMLUtil2.createText(td, value);
    }
    void processListConstraint(Element e, XModelObject o) {
        Element td = XMLUtil.createElement(e, "td"); //$NON-NLS-1$
        XModelObject[] vs = o.getChildren();
        for (int i = 0; i < vs.length; i++) {
            Element s = XMLUtil.createElement(td, "span"); //$NON-NLS-1$
            s.setAttribute("class", "value"); //$NON-NLS-1$ //$NON-NLS-2$
            XMLUtil2.createText(s, vs[i].getAttributeValue(XModelObjectConstants.ATTR_NAME));
            XMLUtil.createElement(td, "br"); //$NON-NLS-1$
        }
    }

    protected void processActionList(Element e, XModelObject o) {
        Element lt = XMLUtil2.createSubTitle(e, "Action List:");
        int i = o.getChildren().length;
        if(i > 0) {
            Element ul = XMLUtil.createElement(lt, "ul"); //$NON-NLS-1$
            processAction(ul, o);
        } else {
            XMLUtil2.createText(lt, "Empty");
        }
    }

    protected void processAction(Element e, XModelObject o) {
        XModelObject[] os = o.getChildren();
        for (int i = 0; i < os.length; i++) {
            if("MetaActionList".equals(os[i].getModelEntity().getName())) { //$NON-NLS-1$
                if("0".equals(os[i].getAttributeValue("group"))) { //$NON-NLS-1$ //$NON-NLS-2$
                    processAction(e, os[i]);
                    XMLUtil2.hr(e, 100);
                } else {
                    Element li = XMLUtil.createElement(e, "li"); //$NON-NLS-1$
                    XMLUtil2.createText(li, os[i].getAttributeValue("display name")); //$NON-NLS-1$
                    Element ul = XMLUtil.createElement(e, "ul"); //$NON-NLS-1$
                    processAction(ul, os[i]);
                }
            } else {
                Element li = XMLUtil.createElement(e, "li"); //$NON-NLS-1$
                XMLUtil2.createText(li, os[i].getAttributeValue("display name")); //$NON-NLS-1$
            }
        }
    }

    class Parents {
        private Hashtable<String,XModelObject> ent = new Hashtable<String,XModelObject>();
        private Hashtable<String,ArrayList<String>> par = new Hashtable<String,ArrayList<String>>();

        public void set(XModelObject o) {
            XModelObject[] cs = o.getChildren("MetaEntityGroup"); //$NON-NLS-1$
            for (int i = 0; i < cs.length; i++) set(cs[i]);
            cs = o.getChildren("MetaEntity"); //$NON-NLS-1$
            for (int i = 0; i < cs.length; i++) {
                String n = cs[i].getAttributeValue(XModelObjectConstants.ATTR_NAME);
                ent.put(n, cs[i]);
                par.put(n, new ArrayList<String>());
            }
        }
        public void compile() {
            Iterator it = ent.values().iterator();
            while(it.hasNext()) {
                XModelObject o = (XModelObject)it.next();
                String p = o.getAttributeValue(XModelObjectConstants.ATTR_NAME);
                XModelObject[] os = o.getChildren("MetaChildren")[0].getChildren(); //$NON-NLS-1$
                for (int i = 0; i < os.length; i++) {
                    String n = os[i].getAttributeValue(XModelObjectConstants.ATTR_NAME);
                    ArrayList<String> v = par.get(n);
                    if(v != null) v.add(p);
                }
            }
        }
        public ArrayList<String> get(XModelObject o) {
            return par.get(o.getAttributeValue(XModelObjectConstants.ATTR_NAME));
        }
    }

    protected void replace() {
        StringBuffer sb = new StringBuffer();
        BufferedReader br = null;
        try {
            br = new BufferedReader(new FileReader(new File(filename)));
            char[] b = new char[256];
            int i = 0;
            while((i = br.read(b, 0, 256)) > 0) {
                sb.append(b, 0, i);
            }
        } catch (IOException e) {
        	ModelPlugin.getPluginLog().logError(e);
        }  finally {
    		try {
    			if(br!=null) {
    				br.close();
    			}
    		} catch (IOException e) {
    			// ignore
    		}
    	}
        int i = 0;
        while(i < sb.length()) {
            if(sb.charAt(i) == '&') sb.replace(i + 1, i + 5, ""); //$NON-NLS-1$
            i++;
        }
        try {
            PrintWriter bw = new PrintWriter(new FileWriter(new File(filename)));
            bw.print(sb.toString());
            bw.flush();
            bw.close();
        } catch (IOException e) {
        	ModelPlugin.getPluginLog().logError(e);
        }
    }

}

class StyleGenerator {
    public String getStyle() {
        return ".name{ font-size: 12pt; text-align: right; vertical-align:top }\n" + //$NON-NLS-1$
               ".value{ font-size: 12pt; font-weight: bold; vertical-align:top }\n" + //$NON-NLS-1$
               ".title{ font-size: 14pt; }\n" + //$NON-NLS-1$
               ".listtab{ font-size: 12pt; margin-left: 20px; vertical-align:top }\n" + //$NON-NLS-1$
               "UL{ list-style: none; margin-left: 20px; }\n"; //$NON-NLS-1$
    }
}

class XMLUtil2 {
    public static void createText(Element e, String text) {
        e.appendChild(e.getOwnerDocument().createTextNode(text));
    }

    public static void hr(Element e) {
        XMLUtil.createElement(e, "hr"); //$NON-NLS-1$
    }

    public static void hr(Element e, int length) {
        Element h = XMLUtil.createElement(e, "hr"); //$NON-NLS-1$
        h.setAttribute("width", "" + length); //$NON-NLS-1$ //$NON-NLS-2$
        h.setAttribute("align", "left"); //$NON-NLS-1$ //$NON-NLS-2$
    }

    public static void simpleRow(Element e, String name, String value) {
        Element tr = XMLUtil.createElement(e, "tr"); //$NON-NLS-1$
        Element td1 = XMLUtil.createElement(tr, "td"); //$NON-NLS-1$
        td1.setAttribute("class", XModelObjectConstants.ATTR_NAME); //$NON-NLS-1$
        createText(td1, name);
        Element td2 = XMLUtil.createElement(tr, "td"); //$NON-NLS-1$
        td1.setAttribute("class", "value"); //$NON-NLS-1$ //$NON-NLS-2$
        createText(td2, value);
    }

    public static Element createSubTitle(Element e, String title) {
        Element p = XMLUtil.createElement(e, "p"); //$NON-NLS-1$
        p.setAttribute("class", "title"); //$NON-NLS-1$ //$NON-NLS-2$
        XMLUtil2.createText(p, title);
        Element lt = XMLUtil.createElement(e, "p"); //$NON-NLS-1$
        lt.setAttribute("class", "listtab"); //$NON-NLS-1$ //$NON-NLS-2$
        return lt;
    }

    public static void entityRow(Element e, String name, String value) {
        Element tr = XMLUtil.createElement(e, "tr"); //$NON-NLS-1$
        Element td1 = XMLUtil.createElement(tr, "td"); //$NON-NLS-1$
        td1.setAttribute("class", XModelObjectConstants.ATTR_NAME); //$NON-NLS-1$
        createText(td1, name);
        Element td2 = XMLUtil.createElement(tr, "td"); //$NON-NLS-1$
        td1.setAttribute("class", "value"); //$NON-NLS-1$ //$NON-NLS-2$
        Element a = XMLUtil.createElement(td2, "a"); //$NON-NLS-1$
        a.setAttribute(XModelObjectConstants.ATTR_NAME, value);
        createText(a, value);
    }

    public static void createEntityReference(Element e, String value) {
        Element a = XMLUtil.createElement(e, "a"); //$NON-NLS-1$
        a.setAttribute("href", "#" + value); //$NON-NLS-1$ //$NON-NLS-2$
        createText(a, value);
    }
}

class ContentGenerator {
    private HashMap<String,XModelObject> list = new HashMap<String,XModelObject>();

    public void generate(Element element, XModelObject o) {
        processGroup(o);
        XMLUtil2.createSubTitle(element, "Entities");
        String[] keys = (String[])list.keySet().toArray(new String[0]);
        Arrays.sort(keys);
        char c = ' ';
        for (int i = 0; i < keys.length; i++) {
            char cx = keys[i].charAt(0);
            if(cx != c) {
                c = cx;
                XMLUtil2.createSubTitle(element, "" + c); //$NON-NLS-1$
            }
            XMLUtil2.createEntityReference(element, keys[i]);
        }
    }

    private void processGroup(XModelObject o) {
        XModelObject[] es = o.getChildren("MetaEntity"); //$NON-NLS-1$
        for (int i = 0; i < es.length; i++) list.put(es[i].getAttributeValue(XModelObjectConstants.ATTR_NAME), es[i]);
        XModelObject[] gs = o.getChildren("MetaEntityGroup"); //$NON-NLS-1$
        for (int i = 0; i < gs.length; i++) processGroup(gs[i]);
    }


}
