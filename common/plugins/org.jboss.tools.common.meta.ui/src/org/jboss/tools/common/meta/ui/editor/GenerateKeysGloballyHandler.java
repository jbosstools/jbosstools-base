package org.jboss.tools.common.meta.ui.editor;

import java.io.File;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Properties;
import java.util.Set;

import org.jboss.tools.common.meta.action.impl.AbstractHandler;
import org.jboss.tools.common.meta.action.impl.handlers.DefaultCreateHandler;
import org.jboss.tools.common.meta.action.impl.handlers.DefaultRemoveHandler;
import org.jboss.tools.common.meta.key.WizardKeys;
import org.jboss.tools.common.model.XModelException;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.filesystems.impl.FolderImpl;
import org.jboss.tools.common.model.util.EclipseResourceUtil;

public class GenerateKeysGloballyHandler extends AbstractHandler {

	//TODO ask in wizard
	String root = "C:/Documents and Settings/glory/RedHat-3.4";

	public GenerateKeysGloballyHandler() {}

    public boolean isEnabled(XModelObject object) {
        return object != null;
    }

    public void executeHandler(XModelObject object, Properties p) throws XModelException {
    	File productRoot = new File(root);
    	File[] plugins = productRoot.listFiles();
    	List<PluginData> relevantPlugins = new ArrayList<PluginData>();
    	Set<String> allKeys = new HashSet<String>();
    	for (int i = 0; i < plugins.length; i++) {
    		if(!plugins[i].isDirectory()) continue;
    		File pxml = new File(plugins[i], "plugin.xml");
    		if(!pxml.isFile()) continue;
    		File meta = new File(plugins[i], "resources/meta");
    		if(!meta.isDirectory()) continue;
			File helpFolder = new File(plugins[i], "resources/help");
			if(!helpFolder.isDirectory()) {
				
				//TODO create folder
			}
    		XModelObject o = EclipseResourceUtil.createObjectForLocation(pxml.getAbsolutePath());
    		if(o == null) {
//    			System.out.println("Null for" + pxml);
    		} else {
    			XModelObject parent = o.getParent();
    			PluginData pluginData = new PluginData();
    			pluginData.rootFolder = plugins[i];
    			pluginData.rootObject = parent;
    			pluginData.metaFolder = parent.getChildByPath("resources/meta");
    			relevantPlugins.add(pluginData);
    			XModelObject[] cs = parent.getChildren();
    			if(!helpFolder.isDirectory()) {
    				System.out.println("No help folder");
    			}
    			XModelObject helpFolderObject = parent.getChildByPath("resources/help");
    			if(helpFolderObject != null) {
    				XModelObject[] hf = helpFolderObject.getChildren("FilePROPERTIES");
    				for (int j = 0; j < hf.length; j++) {
    					XModelObject[] ps = hf[j].getChildren();
    					for (int k = 0; k < ps.length; k++) allKeys.add(ps[k].getAttributeValue("name"));
    				}
    			}
    		}
    		
    		
    	}
    	
    	System.out.println("Total keys count = " + allKeys.size());

    	generateAttributeNameKeys(allKeys, relevantPlugins);

    	generateMenuNameKeys(allKeys, relevantPlugins);
    }

    private void generateAttributeNameKeys(Set<String> allKeys, List<PluginData> relevantPlugins) throws XModelException {
    	for (PluginData d: relevantPlugins) {
    		XModelObject[] ms = d.metaFolder.getChildren("FileMETA");
    		for (int i = 0; i < ms.length; i++) {
        		XModelObject version = ms[i].getChildByPath("Version");
        		String module = version.getAttributeValue("module");
        		
        		String attrKeysFileName = "keys-" + module.toLowerCase() + "-attrs";
        		
        		XModelObject attrKeys = findOrCreatePropertiesFile(d.rootObject, attrKeysFileName);
        		if(attrKeys == null) {
        			continue;
        		}
        		XModelObject[] es = ms[i].getChildren("MetaEntity");
        		for (int j = 0; j < es.length; j++) {
        			XModelObject[] as = es[j].getChildByPath("Attributes").getChildren();
        			for (int k = 0; k < as.length; k++) {
        				if(as[k].getModelEntity().getName().equals("MetaAttribute")) {
        					addAttributeFieldDisplayName(attrKeys, module, allKeys, es[j], as[k]);
        				}
        				
        			}
        		}
    			if(attrKeys.getChildren().length > 0) {
    				System.out.println(attrKeysFileName + " " + attrKeys.getChildren().length);
    				((FolderImpl)attrKeys.getParent()).saveChild(attrKeys);
    			}
    		}
    	}
    }

    private void addAttributeFieldDisplayName(XModelObject attrKeys, String module, Set<String> allKeys, XModelObject ent, XModelObject attr) throws XModelException {
		String name = attr.getAttributeValue("name");
		String key_att_1 = ent.getAttributeValue("name") + "_" + name.replace(' ', '_');
		if(allKeys.contains(key_att_1)) {
//			System.out.println("--->" + key_att_1);
			return;
		}
		key_att_1 = ent.getAttributeValue("name") + "." + name.replace(' ', '_');
		if(allKeys.contains(key_att_1)) {
//			System.out.println("--->" + key_att_1);
			return;
		}
		String key_att_2 = module + "." + name.replace(' ', '_').replace(':', '_');
		boolean invisible = "false".equals(attr.getAttributeValue("visibility"));
		if(attrKeys.getChildByPath(key_att_2) != null) {
			if(invisible) {
//				DefaultRemoveHandler.removeFromParent(attrKeys.getChildByPath(key_att_2));
			}
			return;
		}
		if(invisible) return;
		XModelObject po = attrKeys.getModel().createModelObject("Property", new Properties());
		po.setAttributeValue("name", key_att_2);
		po.setAttributeValue("value", WizardKeys.toDisplayName(name));
		DefaultCreateHandler.addCreatedObject(attrKeys, po, 2);
    }

    private void generateMenuNameKeys(Set<String> allKeys, List<PluginData> relevantPlugins) throws XModelException {
		Set<String> mk = new HashSet<String>(); //Temp
    	for (PluginData d: relevantPlugins) {
    		XModelObject[] ms = d.metaFolder.getChildren("FileMETA");
    		for (int i = 0; i < ms.length; i++) {
        		XModelObject version = ms[i].getChildByPath("Version");
        		String module = version.getAttributeValue("module");
        		
        		String menuKeysFileName = "keys-" + module.toLowerCase() + "-menu";
        		
        		XModelObject menuKeys = findOrCreatePropertiesFile(d.rootObject, menuKeysFileName);
        		if(menuKeys == null) {
        			continue;
        		}
        		XModelObject[] es = ms[i].getChildren("MetaEntity");
        		for (int j = 0; j < es.length; j++) {
        			XModelObject list = es[j].getChildByPath("ActionList");
        			if(list != null) {
        				generateMenuNameKeys(allKeys, menuKeys, list, mk);
        			}
        		}
    			if(menuKeys.getChildren().length > 0) {
    				System.out.println(menuKeysFileName + " " + menuKeys.getChildren().length);
    				((FolderImpl)menuKeys.getParent()).saveChild(menuKeys);
    			}
    		}
    	}
    	System.out.println("action menu names " + mk.size());
    	for (String s: mk) System.out.println(s);
    }

    private void generateMenuNameKeys(Set<String> allKeys, XModelObject menuKeys, XModelObject list, Set<String> mk) throws XModelException {
    	String n = list.getAttributeValue("name");
    	String g = list.getAttributeValue("group");
    	if("1".equals(g)) {
    		mk.add(n);
    	}
    	XModelObject[] cs = list.getChildren();
    	for (int i = 0; i < cs.length; i++) {
    		String entity = cs[i].getModelEntity().getName();
    		if("MetaActionList".equals(entity)) {
    			generateMenuNameKeys(allKeys, menuKeys, cs[i], mk);
    		} else {
    			String na = cs[i].getAttributeValue("name");
    			mk.add(na);
    		}
    	}
    }

    private XModelObject findOrCreatePropertiesFile(XModelObject root, String name) {
    	XModelObject parent = root.getChildByPath("resources/help");
    	if(parent == null) return null;
    	XModelObject o = parent.getChildByPath(name + ".properties");
    	if(o == null) {
    		o = parent.getModel().createModelObject("FilePROPERTIES", new Properties());
    		o.setAttributeValue("name", name);
    		parent.addChild(o);
    		parent.setModified(true);
    	}
    	return o;
    }

    class PluginData {
    	File rootFolder;
    	XModelObject rootObject;
    	XModelObject metaFolder;
    }
}
