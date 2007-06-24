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

import java.io.*;
import java.util.*;
import org.w3c.dom.*;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.jboss.tools.common.model.util.*;

public class DocumentGenerator {
    private XModelObject meta = null;
    private String filename = null;
    private Parents parents = null;

    public DocumentGenerator() {}

    public void setModel(XModel model) {
        meta = model.getRoot().getChildByPath("MetaModel");
    }

    public void generate(String filename) {
        if(meta == null) throw new RuntimeException("Meta root is not set.");
        this.filename = filename;
        Element g = XMLUtil.createDocumentElement("html");
        generateTitle(g);
        generateHead(g);
        generateBody(g);
        try {
            XModelObjectLoaderUtil.serialize(g, filename);
            replace();
        } catch (Exception e) {
        	ModelPlugin.log(e);
        }
    }

    protected void generateTitle(Element e) {
        Element t = XMLUtil.createElement(e, "title");
        XMLUtil2.createText(t, "Entity list");
    }

    protected void generateHead(Element e) {
        Element h = XMLUtil.createElement(e, "head");
        Element s = XMLUtil.createElement(h, "style");
        XMLUtil2.createText(s, new StyleGenerator().getStyle());
    }

    protected void generateBody(Element e) {
        Element b = XMLUtil.createElement(e, "body");
        XModelObject entities = meta.getChildren("MetaEntities")[0];
        parents = new Parents();
        parents.set(entities);
        parents.compile();
        new ContentGenerator().generate(b, entities);
        processGroup(b, entities);
    }

    protected void processGroup(Element e, XModelObject o) {
        XModelObject[] es = o.getChildren("MetaEntity");
        for (int i = 0; i < es.length; i++) processEntity(e, es[i]);
        XModelObject[] gs = o.getChildren("MetaEntityGroup");
        for (int i = 0; i < gs.length; i++) processGroup(e, gs[i]);
    }

    protected void processEntity(Element e, XModelObject o) {
        XMLUtil2.hr(e);
        Element t = XMLUtil.createElement(e, "table");
        XMLUtil2.entityRow(t, "Name:", o.getAttributeValue("name"));
        XMLUtil2.simpleRow(t, "Editor:", o.getAttributeValue("editor"));
        processHierarchy(e, o);
        processAttributeList(e, o.getChildren("MetaAttributes")[0]);
        processActionList(e, o.getChildren("MetaActionList")[0]);
    }

    protected void processHierarchy(Element e, XModelObject o) {
        Element t = XMLUtil.createElement(e, "table");
        t.setAttribute("width", "600");
        Element tr = XMLUtil.createElement(t, "tr");
        for (int i = 0; i < 2; i++) {
            Element td = XMLUtil.createElement(tr, "td");
            td.setAttribute("width", "300");
        }
        tr = XMLUtil.createElement(t, "tr");
        Element td = XMLUtil.createElement(tr, "td");
        td.setAttribute("valign", "top");
        processParents(td, o);
        td = XMLUtil.createElement(tr, "td");
        td.setAttribute("valign", "top");
        processChildren(td, o.getChildren("MetaChildren")[0]);

    }

    protected void processParents(Element e, XModelObject o) {
        Element lt = XMLUtil2.createSubTitle(e, "Can be inside:");
        ArrayList<String> v = parents.get(o);
        if(v.size() == 0) {
            XMLUtil2.createText(lt, "None");
            XMLUtil.createElement(lt, "br");
        } else for (int i = 0; i < v.size(); i++) {
            Element s = XMLUtil.createElement(lt, "span");
            s.setAttribute("class", "value");
            XMLUtil2.createEntityReference(s, v.get(i));
            XMLUtil.createElement(lt, "br");
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
        Element tr = XMLUtil.createElement(t, "tr");
        createHeaderCell(tr, "Name", 0);
        createHeaderCell(tr, "Required", 0);
        for (int i = 0; i < as.length; i++) {
            tr = XMLUtil.createElement(tr, "tr");
            Element td = XMLUtil.createElement(tr, "td");
            td.setAttribute("class", "value");
            XMLUtil2.createEntityReference(td, as[i].getAttributeValue("name"));
            createValueCell(tr, as[i].getAttributeValue("required"));
        }
    }

    protected void processAttributeList(Element e, XModelObject o) {
        Element lt = XMLUtil2.createSubTitle(e, "Attribute List:");
        Element t = createTable(lt);
        Element tr = XMLUtil.createElement(t, "tr");
        createHeaderCell(tr, "Name", 130);
        createHeaderCell(tr, "Visible", 50);
        createHeaderCell(tr, "Editor", 0);
        createHeaderCell(tr, "Default Value", 0);
        createHeaderCell(tr, "Possible Values", 0);
        XModelObject[] as = o.getChildren("MetaAttribute");
        for (int i = 0; i < as.length; i++) {
            tr = XMLUtil.createElement(tr, "tr");
            createValueCell(tr, as[i].getAttributeValue("name"));
            createValueCell(tr, as[i].getAttributeValue("visibility"));
            XModelObject oe = as[i].getChildren("MetaAttributeEditor")[0];
            String ed = oe.getAttributeValue("name");
            createValueCell(tr, ed);
            String dv = as[i].getAttributeValue("default value");
            createValueCell(tr, dv);
            XModelObject oc = as[i].getChildren("MetaAttributeConstraint")[0];
            String cl = oc.getAttributeValue("loader");
            if(cl.startsWith("List")) {
                processListConstraint(tr, oc);
            } else if(cl.length() == 0 && "Uneditable".equals(ed)) {
                createValueCell(tr, dv);
            } else if(cl.length() == 0 && "Int".equals(ed)) {
                createValueCell(tr, "integer");
            } else {
                createValueCell(tr, "any");
            }
        }
    }

    Element createTable(Element e) {
        Element t = XMLUtil.createElement(e, "table");
        t.setAttribute("border", "1");
        t.setAttribute("cellspacing", "0");
        t.setAttribute("cellpadding", "2");
        return t;
    }
    void createHeaderCell(Element e, String name, int width) {
        Element td = XMLUtil.createElement(e, "td");
        td.setAttribute("class", "name");
        if(width > 0) td.setAttribute("width", "" + width);
        XMLUtil2.createText(td, name);
    }
    void createValueCell(Element e, String value) {
        Element td = XMLUtil.createElement(e, "td");
        if(value.length() == 0) value = "&nbsp;";
        td.setAttribute("class", "value");
        XMLUtil2.createText(td, value);
    }
    void processListConstraint(Element e, XModelObject o) {
        Element td = XMLUtil.createElement(e, "td");
        XModelObject[] vs = o.getChildren();
        for (int i = 0; i < vs.length; i++) {
            Element s = XMLUtil.createElement(td, "span");
            s.setAttribute("class", "value");
            XMLUtil2.createText(s, vs[i].getAttributeValue("name"));
            XMLUtil.createElement(td, "br");
        }
    }

    protected void processActionList(Element e, XModelObject o) {
        Element lt = XMLUtil2.createSubTitle(e, "Action List:");
        int i = o.getChildren().length;
        if(i > 0) {
            Element ul = XMLUtil.createElement(lt, "ul");
            processAction(ul, o);
        } else {
            XMLUtil2.createText(lt, "Empty");
        }
    }

    protected void processAction(Element e, XModelObject o) {
        XModelObject[] os = o.getChildren();
        for (int i = 0; i < os.length; i++) {
            if("MetaActionList".equals(os[i].getModelEntity().getName())) {
                if("0".equals(os[i].getAttributeValue("group"))) {
                    processAction(e, os[i]);
                    XMLUtil2.hr(e, 100);
                } else {
                    Element li = XMLUtil.createElement(e, "li");
                    XMLUtil2.createText(li, os[i].getAttributeValue("display name"));
                    Element ul = XMLUtil.createElement(e, "ul");
                    processAction(ul, os[i]);
                }
            } else {
                Element li = XMLUtil.createElement(e, "li");
                XMLUtil2.createText(li, os[i].getAttributeValue("display name"));
            }
        }
    }

    class Parents {
        private Hashtable<String,XModelObject> ent = new Hashtable<String,XModelObject>();
        private Hashtable<String,ArrayList<String>> par = new Hashtable<String,ArrayList<String>>();

        public void set(XModelObject o) {
            XModelObject[] cs = o.getChildren("MetaEntityGroup");
            for (int i = 0; i < cs.length; i++) set(cs[i]);
            cs = o.getChildren("MetaEntity");
            for (int i = 0; i < cs.length; i++) {
                String n = cs[i].getAttributeValue("name");
                ent.put(n, cs[i]);
                par.put(n, new ArrayList<String>());
            }
        }
        public void compile() {
            Iterator it = ent.values().iterator();
            while(it.hasNext()) {
                XModelObject o = (XModelObject)it.next();
                String p = o.getAttributeValue("name");
                XModelObject[] os = o.getChildren("MetaChildren")[0].getChildren();
                for (int i = 0; i < os.length; i++) {
                    String n = os[i].getAttributeValue("name");
                    ArrayList<String> v = par.get(n);
                    if(v != null) v.add(p);
                }
            }
        }
        public ArrayList<String> get(XModelObject o) {
            return par.get(o.getAttributeValue("name"));
        }
    }

    protected void replace() {
        StringBuffer sb = new StringBuffer();
        try {
            BufferedReader br = new BufferedReader(new FileReader(new File(filename)));
            char[] b = new char[256];
            int i = 0;
            while((i = br.read(b, 0, 256)) > 0) {
                sb.append(b, 0, i);
            }
        } catch (Exception e) {
        	ModelPlugin.log(e);
        }
        int i = 0;
        while(i < sb.length()) {
            if(sb.charAt(i) == '&') sb.replace(i + 1, i + 5, "");
            i++;
        }
        try {
            PrintWriter bw = new PrintWriter(new FileWriter(new File(filename)));
            bw.print(sb.toString());
            bw.flush();
            bw.close();
        } catch (Exception e) {
        	ModelPlugin.log(e);
        }
    }

}

class StyleGenerator {
    public String getStyle() {
        return ".name{ font-size: 12pt; text-align: right; vertical-align:top }\n" +
               ".value{ font-size: 12pt; font-weight: bold; vertical-align:top }\n" +
               ".title{ font-size: 14pt; }\n" +
               ".listtab{ font-size: 12pt; margin-left: 20px; vertical-align:top }\n" +
               "UL{ list-style: none; margin-left: 20px; }\n";
    }
}

class XMLUtil2 {
    public static void createText(Element e, String text) {
        e.appendChild(e.getOwnerDocument().createTextNode(text));
    }

    public static void hr(Element e) {
        XMLUtil.createElement(e, "hr");
    }

    public static void hr(Element e, int length) {
        Element h = XMLUtil.createElement(e, "hr");
        h.setAttribute("width", "" + length);
        h.setAttribute("align", "left");
    }

    public static void simpleRow(Element e, String name, String value) {
        Element tr = XMLUtil.createElement(e, "tr");
        Element td1 = XMLUtil.createElement(tr, "td");
        td1.setAttribute("class", "name");
        createText(td1, name);
        Element td2 = XMLUtil.createElement(tr, "td");
        td1.setAttribute("class", "value");
        createText(td2, value);
    }

    public static Element createSubTitle(Element e, String title) {
        Element p = XMLUtil.createElement(e, "p");
        p.setAttribute("class", "title");
        XMLUtil2.createText(p, title);
        Element lt = XMLUtil.createElement(e, "p");
        lt.setAttribute("class", "listtab");
        return lt;
    }

    public static void entityRow(Element e, String name, String value) {
        Element tr = XMLUtil.createElement(e, "tr");
        Element td1 = XMLUtil.createElement(tr, "td");
        td1.setAttribute("class", "name");
        createText(td1, name);
        Element td2 = XMLUtil.createElement(tr, "td");
        td1.setAttribute("class", "value");
        Element a = XMLUtil.createElement(td2, "a");
        a.setAttribute("name", value);
        createText(a, value);
    }

    public static void createEntityReference(Element e, String value) {
        Element a = XMLUtil.createElement(e, "a");
        a.setAttribute("href", "#" + value);
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
                XMLUtil2.createSubTitle(element, "" + c);
            }
            XMLUtil2.createEntityReference(element, keys[i]);
        }
    }

    private void processGroup(XModelObject o) {
        XModelObject[] es = o.getChildren("MetaEntity");
        for (int i = 0; i < es.length; i++) list.put(es[i].getAttributeValue("name"), es[i]);
        XModelObject[] gs = o.getChildren("MetaEntityGroup");
        for (int i = 0; i < gs.length; i++) processGroup(gs[i]);
    }


}
