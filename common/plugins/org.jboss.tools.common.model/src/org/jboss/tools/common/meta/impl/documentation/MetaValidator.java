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

import java.text.MessageFormat;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.Set;
import java.util.Vector;

import org.jboss.tools.common.model.XModelObjectConstants;
import org.jboss.tools.common.model.XModelObject;

public class MetaValidator {
    private IconsValidator iv = new IconsValidator();
    private EntitiesValidator ev = new EntitiesValidator();
//    private MappingsValidator mv = new MappingsValidator();

    public MetaValidator() {}

    public void validate(XModelObject metaroot) {
        if(!"MetaRoot".equals(metaroot.getModelEntity().getName())) //$NON-NLS-1$
          throw new IllegalArgumentException("Meta validator must be called with MetaRoot."); //$NON-NLS-1$
        iv.validate(metaroot.getChildren("MetaIcons")[0]); //$NON-NLS-1$
        ev.validate(metaroot.getChildren("MetaEntities")[0]); //$NON-NLS-1$
////        mv.validate(metaroot.getChildren("MetaMappings")[0]);
    }

    public static final void message(String message) {
		///XStudioPlugin.log(message);
    }

    public static final String id(XModelObject object) {
        return object.getAttributeValue(XModelObjectConstants.ATTR_ELEMENT_TYPE) + " " + //$NON-NLS-1$
               object.getModelEntity().getRenderer().getTitle(object);
    }

    public static boolean isBasic(XModelObject object) {
        String e = object.getModelEntity().getName();
        return ("MetaEntity".equals(e) || "MetaMapping".equals(e)); //$NON-NLS-1$ //$NON-NLS-2$
    }

    public static String entityId(XModelObject object) {
        while(object != null) {
            if(isBasic(object)) return id(object);
            object = object.getParent();
        }
        return "!!!!!!!!!!!!"; //$NON-NLS-1$
    }

    public static final String longid(XModelObject object) {
        return id(object) + ((isBasic(object)) ? "" : MessageFormat.format(" of {0}", entityId(object))); //$NON-NLS-1$
    }

    public static final void checkClass(XModelObject object, String attribute, boolean mandatory, String mapping) {
        checkClass(object, attribute, "", mandatory, mapping); //$NON-NLS-1$
    }

    public static final void checkClass(XModelObject object, String attribute, String prefix, boolean mandatory, String mapping) {
        String pref = MessageFormat.format("Error in attribute ''{0}'' in {1}: ", attribute,
				longid(object));
        String classname = object.getAttributeValue(attribute);
        int i = classname.indexOf('%');
        if(classname.length() == 0) {
            if(!mandatory) return;
            message(pref + " class name cannot be empty.");
        } else if(i == 0) {
            if(mapping == null) {
                message(pref + (MessageFormat.format(" alias {0} is not allowed.", classname)));
            } else if(classname.charAt(classname.length() - 1) != '%') {
                message(pref + (MessageFormat.format(" incorrect alias {0}", classname)));
            } else {
                MappingsValidator.classmappings.add(mapping);
                String alias = classname.substring(1, classname.length() - 1);
                XModelObject v = object.getModel().getByPath("MetaModel/Mappings/" + mapping); //$NON-NLS-1$
                if(v == null) message(MessageFormat.format("Mapping {0} is not found.", mapping));
                if(v.getChildByPath(alias) != null) return;
                message(pref + (MessageFormat.format(" incorrect alias {0}", classname)));
            }
        } else {
            try {
                Class.forName(prefix + classname).newInstance();
            } catch (ClassNotFoundException e) {
            	  message(pref + (MessageFormat.format(" class ''{0}'' not found.", classname)));              
            } catch (InstantiationException e) {
            	  message(pref + (MessageFormat.format(" class ''{0}'' not found.", classname)));
			} catch (IllegalAccessException e) {
				  message(pref + (MessageFormat.format(" class ''{0}'' not found.", classname)));
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
        XModelObject[] gs = object.getChildren("MetaIconGroup"); //$NON-NLS-1$
        for (int i = 0; i < gs.length; i++) validate2(gs[i]);
        XModelObject[] is = object.getChildren("MetaIcon"); //$NON-NLS-1$
        for (int i = 0; i < is.length; i++) {
            String s = is[i].getPath().substring(iconsrootlength).replace('/', '.');
            String p = is[i].getAttributeValue("path"); //$NON-NLS-1$
            icons.addElement(s);
            if(int.class.getResource(p) == null)
              MetaValidator.message(MessageFormat.format(
					"Error in icon {0}: Resource {1} is not found.", s,
					p));
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
        XModelObject[] gs = object.getChildren("MetaEntityGroup"); //$NON-NLS-1$
        for (int i = 0; i < gs.length; i++) validate(gs[i]);
        XModelObject[] es = object.getChildren("MetaEntity"); //$NON-NLS-1$
        for (int i = 0; i < es.length; i++) {
            MetaValidator.checkClass(es[i], "implementation", true, null); //$NON-NLS-1$
            MetaValidator.checkClass(es[i], "loader", false, null); //$NON-NLS-1$
            MetaValidator.checkClass(es[i], "generator", false, null); //$NON-NLS-1$
            MetaValidator.checkClass(es[i], "editor", false, "ObjectEditor"); //$NON-NLS-1$ //$NON-NLS-2$
            MetaValidator.checkClass(es[i], "adopt manager", false, null); //$NON-NLS-1$
            cv.validate(es[i]);
            av.validate(es[i]);
            tv.validate(es[i]);
        }
    }

    private void collect(XModelObject object) {
        XModelObject[] gs = object.getChildren("MetaEntityGroup"); //$NON-NLS-1$
        for (int i = 0; i < gs.length; i++) collect(gs[i]);
        XModelObject[] es = object.getChildren("MetaEntity"); //$NON-NLS-1$
        for (int i = 0; i < es.length; i++) {
            XModelObject[] as = es[i].getChildByPath("Attributes").getChildren(); //$NON-NLS-1$
            Vector<String> v = new Vector<String>();
            for (int j = 0; j < as.length; j++) v.addElement(as[j].getAttributeValue(XModelObjectConstants.ATTR_NAME));
            entities.put(es[i].getAttributeValue(XModelObjectConstants.ATTR_NAME), v);
        }
    }
}

class ChildrenValidator {
    public void validate(XModelObject object) {
        XModelObject[] cs = object.getChildByPath("Children").getChildren(); //$NON-NLS-1$
        for (int i = 0; i < cs.length; i++) {
            String c = cs[i].getAttributeValue(XModelObjectConstants.ATTR_NAME);
            if(EntitiesValidator.entities.get(c) == null) {
                MetaValidator.message(MessageFormat
						.format(
								"Error in {0}: child {1} not found in the entity list.",
								MetaValidator.id(object), c));
            }
        }
    }
}

class AttributesValidator {

    public void validate(XModelObject object) {
        XModelObject[] as = object.getChildByPath("Attributes").getChildren(); //$NON-NLS-1$
        for (int i = 0; i < as.length; i++) {
            String an = as[i].getAttributeValue(XModelObjectConstants.ATTR_NAME);
            MetaValidator.checkClass(as[i], "loader", "org.jboss.tools.common.meta.impl.adapters.XAdapter", false, null); //$NON-NLS-1$ //$NON-NLS-2$
            XModelObject ed = as[i].getChildren("MetaAttributeEditor")[0]; //$NON-NLS-1$
            String en = ed.getAttributeValue(XModelObjectConstants.ATTR_NAME);
            if("GUI".equals(en)) continue; //$NON-NLS-1$
            XModelObject mi = ed.getModel().getByPath("MetaModel/Mappings/AttributeEditor/" + en); //$NON-NLS-1$
            if(mi == null) {
                MetaValidator.message(MessageFormat
						.format(
								"Error in attribute ''{0}'' of {1}: incorrect editor ''{2}''.",
								an, MetaValidator.id(object), en));
            }
        }
    }
}

class ActionsValidator {
    private EntityDataValidator dv = new EntityDataValidator();

    public void validate(XModelObject object) {
        XModelObject[] ls = object.getChildren("MetaActionList"); //$NON-NLS-1$
        for (int i = 0; i < ls.length; i++) validate(ls[i]);
        XModelObject[] as = object.getChildren("MetaAction"); //$NON-NLS-1$
        for (int i = 0; i < as.length; i++) {
//            String n = as[i].getAttributeValue("display name");
            String in = as[i].getAttributeValue("icon"); //$NON-NLS-1$
            if(in.trim().length() == 0)
              MetaValidator.message(MessageFormat.format("Error in {0}: icon not specified", MetaValidator.longid(as[i])));
            if(in.length() > 0 && !IconsValidator.icons.contains(in))
              MetaValidator.message(MessageFormat.format("Error in {0}: icon ''{1}'' not found",
					MetaValidator.longid(as[i]), in));
            MetaValidator.checkClass(as[i], "handler", true, null); //$NON-NLS-1$
            String wm = (System.getProperty("testmodel") != null) ? "TestWizards" : "Wizards"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
            MetaValidator.checkClass(as[i], "wizard", false, wm); //$NON-NLS-1$
            dv.validate(as[i]);
        }
    }
}

class EntityDataValidator {
    public void validate(XModelObject object) {
        XModelObject[] ds = object.getChildren("MetaEntityData"); //$NON-NLS-1$
        for (int i = 0; i < ds.length; i++) {
            String en = ds[i].getAttributeValue("entity name"); //$NON-NLS-1$
            Vector v = (Vector)EntitiesValidator.entities.get(en);
            if(v == null) {
                MetaValidator.message(MessageFormat.format("Error in {0}: entity ''{1}'' not found.",
						MetaValidator.longid(object), en));
            } else {
                XModelObject[] as = ds[i].getChildren("MetaAttributeData"); //$NON-NLS-1$
                for (int j = 0; j < as.length; j++) {
                    String an = as[j].getAttributeValue("attribute name"); //$NON-NLS-1$
                    if(v.contains(an)) continue;
                    MetaValidator.message(MessageFormat
							.format(
									"Error in {0}: attribute ''{1}'' not found in entity {2}.",
									MetaValidator.longid(object), an, en));
                }
            }
        }
    }
}

class MappingsValidator {
    private MappingToClassValidator cv = new MappingToClassValidator();
    public static Set<String> classmappings = new HashSet<String>();

    public void validate(XModelObject object) {
        classmappings.add("FilteredTrees"); //$NON-NLS-1$
        if(System.getProperty("testmodel") != null) { //$NON-NLS-1$
            classmappings.remove("ObjectEditor"); //$NON-NLS-1$
        } else {
            classmappings.add("AttributeEditor"); //$NON-NLS-1$
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
          MetaValidator.checkClass(is[i], "value", true, null); //$NON-NLS-1$
    }
}

