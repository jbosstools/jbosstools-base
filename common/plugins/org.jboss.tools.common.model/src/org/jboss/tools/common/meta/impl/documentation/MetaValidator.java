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

import java.util.*;
import org.jboss.tools.common.model.XModelObject;

public class MetaValidator {
    private IconsValidator iv = new IconsValidator();
    private EntitiesValidator ev = new EntitiesValidator();
//    private MappingsValidator mv = new MappingsValidator();

    public MetaValidator() {}

    public void validate(XModelObject metaroot) {
        if(!"MetaRoot".equals(metaroot.getModelEntity().getName()))
          throw new RuntimeException("Meta validator must be called with MetaRoot.");
        iv.validate(metaroot.getChildren("MetaIcons")[0]);
        ev.validate(metaroot.getChildren("MetaEntities")[0]);
////        mv.validate(metaroot.getChildren("MetaMappings")[0]);
    }

    public static final void message(String message) {
		///XStudioPlugin.log(message);
    }

    public static final String id(XModelObject object) {
        return object.getAttributeValue("element type") + " " +
               object.getModelEntity().getRenderer().getTitle(object);
    }

    public static boolean isBasic(XModelObject object) {
        String e = object.getModelEntity().getName();
        return ("MetaEntity".equals(e) || "MetaMapping".equals(e));
    }

    public static String entityId(XModelObject object) {
        while(object != null) {
            if(isBasic(object)) return id(object);
            object = object.getParent();
        }
        return "!!!!!!!!!!!!";
    }

    public static final String longid(XModelObject object) {
        return id(object) + ((isBasic(object)) ? "" : " of " + entityId(object));
    }

    public static final void checkClass(XModelObject object, String attribute, boolean mandatory, String mapping) {
        checkClass(object, attribute, "", mandatory, mapping);
    }

    public static final void checkClass(XModelObject object, String attribute, String prefix, boolean mandatory, String mapping) {
        String pref = "Error in attribute '" + attribute + "' in " + longid(object) + ": ";
        String classname = object.getAttributeValue(attribute);
        int i = classname.indexOf('%');
        if(classname.length() == 0) {
            if(!mandatory) return;
            message(pref + " class name cannot be empty.");
        } else if(i == 0) {
            if(mapping == null) {
                message(pref + " alias " + classname + " is not allowed.");
            } else if(classname.charAt(classname.length() - 1) != '%') {
                message(pref + " incorrect alias " + classname);
            } else {
                MappingsValidator.classmappings.add(mapping);
                String alias = classname.substring(1, classname.length() - 1);
                XModelObject v = object.getModel().getByPath("MetaModel/Mappings/" + mapping);
                if(v == null) message("Mapping " + mapping + " is not found.");
                if(v.getChildByPath(alias) != null) return;
                message(pref + " incorrect alias " + classname);
            }
        } else {
            try {
                Class.forName(prefix + classname).newInstance();
            } catch (Exception e) {
                message(pref + " class '" + classname + "' not found.");
            }
        }
    }
}

class IconsValidator {
    public static final Vector<String> icons = new Vector<String>();
    private int iconsrootlength = 0;

    public void validate(XModelObject object) {
        iconsrootlength = object.getPath().length() + 1;
        validate2(object);
    }

    private void validate2(XModelObject object) {
        XModelObject[] gs = object.getChildren("MetaIconGroup");
        for (int i = 0; i < gs.length; i++) validate2(gs[i]);
        XModelObject[] is = object.getChildren("MetaIcon");
        for (int i = 0; i < is.length; i++) {
            String s = is[i].getPath().substring(iconsrootlength).replace('/', '.');
            String p = is[i].getAttributeValue("path");
            icons.addElement(s);
            if(int.class.getResource(p) == null)
              MetaValidator.message("Error in icon " + s + ": Resource " + p + " is not found.");
        }
    }

}

class EntitiesValidator {
    public static final Hashtable<String,Vector<String>> entities = new Hashtable<String,Vector<String>>();
    private ChildrenValidator cv = new ChildrenValidator();
    private AttributesValidator av = new AttributesValidator();
    private ActionsValidator tv = new ActionsValidator();

    public void validate(XModelObject object) {
        collect(object);
        XModelObject[] gs = object.getChildren("MetaEntityGroup");
        for (int i = 0; i < gs.length; i++) validate(gs[i]);
        XModelObject[] es = object.getChildren("MetaEntity");
        for (int i = 0; i < es.length; i++) {
            MetaValidator.checkClass(es[i], "implementation", true, null);
            MetaValidator.checkClass(es[i], "loader", false, null);
            MetaValidator.checkClass(es[i], "generator", false, null);
            MetaValidator.checkClass(es[i], "editor", false, "ObjectEditor");
            MetaValidator.checkClass(es[i], "adopt manager", false, null);
            cv.validate(es[i]);
            av.validate(es[i]);
            tv.validate(es[i]);
        }
    }

    private void collect(XModelObject object) {
        XModelObject[] gs = object.getChildren("MetaEntityGroup");
        for (int i = 0; i < gs.length; i++) collect(gs[i]);
        XModelObject[] es = object.getChildren("MetaEntity");
        for (int i = 0; i < es.length; i++) {
            XModelObject[] as = es[i].getChildByPath("Attributes").getChildren();
            Vector<String> v = new Vector<String>();
            for (int j = 0; j < as.length; j++) v.addElement(as[j].getAttributeValue("name"));
            entities.put(es[i].getAttributeValue("name"), v);
        }
    }
}

class ChildrenValidator {
    public void validate(XModelObject object) {
        XModelObject[] cs = object.getChildByPath("Children").getChildren();
        for (int i = 0; i < cs.length; i++) {
            String c = cs[i].getAttributeValue("name");
            if(EntitiesValidator.entities.get(c) == null) {
                MetaValidator.message("Error in " + MetaValidator.id(object) +
                             ": child " + c + " not found in the entity list.");
            }
        }
    }
}

class AttributesValidator {

    public void validate(XModelObject object) {
        XModelObject[] as = object.getChildByPath("Attributes").getChildren();
        for (int i = 0; i < as.length; i++) {
            String an = as[i].getAttributeValue("name");
            MetaValidator.checkClass(as[i], "loader", "org.jboss.tools.common.meta.impl.adapters.XAdapter", false, null);
            XModelObject ed = as[i].getChildren("MetaAttributeEditor")[0];
            String en = ed.getAttributeValue("name");
            if("GUI".equals(en)) continue;
            XModelObject mi = ed.getModel().getByPath("MetaModel/Mappings/AttributeEditor/" + en);
            if(mi == null) {
                MetaValidator.message("Error in attribute '" + an + "' of " +
                              MetaValidator.id(object) + ": incorrect editor '" + en + "'.");
            }
        }
    }
}

class ActionsValidator {
    private EntityDataValidator dv = new EntityDataValidator();

    public void validate(XModelObject object) {
        XModelObject[] ls = object.getChildren("MetaActionList");
        for (int i = 0; i < ls.length; i++) validate(ls[i]);
        XModelObject[] as = object.getChildren("MetaAction");
        for (int i = 0; i < as.length; i++) {
//            String n = as[i].getAttributeValue("display name");
            String in = as[i].getAttributeValue("icon");
            if(in.trim().length() == 0)
              MetaValidator.message("Error in " + MetaValidator.longid(as[i]) +
                                    ": icon not specified");
            if(in.length() > 0 && !IconsValidator.icons.contains(in))
              MetaValidator.message("Error in " + MetaValidator.longid(as[i]) +
                                    ": icon '" + in + "' not found");
            MetaValidator.checkClass(as[i], "handler", true, null);
            String wm = (System.getProperty("testmodel") != null) ? "TestWizards" : "Wizards";
            MetaValidator.checkClass(as[i], "wizard", false, wm);
            dv.validate(as[i]);
        }
    }
}

class EntityDataValidator {
    public void validate(XModelObject object) {
        XModelObject[] ds = object.getChildren("MetaEntityData");
        for (int i = 0; i < ds.length; i++) {
            String en = ds[i].getAttributeValue("entity name");
            Vector v = (Vector)EntitiesValidator.entities.get(en);
            if(v == null) {
                MetaValidator.message("Error in " + MetaValidator.longid(object) +
                              ": entity '" + en + "' not found.");
            } else {
                XModelObject[] as = ds[i].getChildren("MetaAttributeData");
                for (int j = 0; j < as.length; j++) {
                    String an = as[j].getAttributeValue("attribute name");
                    if(v.contains(an)) continue;
                    MetaValidator.message("Error in " + MetaValidator.longid(object) +
                        ": attribute '" + an + "' not found in entity " + en + ".");
                }
            }
        }
    }
}

class MappingsValidator {
    private MappingToClassValidator cv = new MappingToClassValidator();
    public static Set<String> classmappings = new HashSet<String>();

    public void validate(XModelObject object) {
        classmappings.add("FilteredTrees");
        if(System.getProperty("testmodel") != null) {
            classmappings.remove("ObjectEditor");
        } else {
            classmappings.add("AttributeEditor");
        }
        Object[] mtoc = classmappings.toArray();
        for (int i = 0; i < mtoc.length; i++) {
            cv.validate(object.getChildByPath((String)mtoc[i]));
        }
    }
}


class MappingToClassValidator {
    public void validate(XModelObject object) {
        if(object == null) return;
        XModelObject[] is = object.getChildren();
        for (int i = 0; i < is.length; i++)
          MetaValidator.checkClass(is[i], "value", true, null);
    }
}

