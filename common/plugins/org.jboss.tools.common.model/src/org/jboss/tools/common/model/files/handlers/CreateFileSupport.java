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
package org.jboss.tools.common.model.files.handlers;

import java.io.*;
import java.util.*;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.*;
import org.eclipse.swt.widgets.Display;

import org.jboss.tools.common.meta.XMapping;
import org.jboss.tools.common.meta.XModelMetaData;
import org.jboss.tools.common.meta.action.*;
import org.jboss.tools.common.meta.action.impl.*;
import org.jboss.tools.common.meta.action.impl.handlers.DefaultCreateHandler;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.filesystems.impl.*;
import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.jboss.tools.common.model.util.*;
import org.jboss.tools.common.util.FileUtil;

public class CreateFileSupport extends SpecialWizardSupport {
	protected TargetHolder targetHolder = new TargetHolder();
	TreeMap<String,String> versionEntities = new TreeMap<String,String>();
	boolean useVersions = false;
	
	public void reset() {
		if(hasTemplate()) {
			String[] s = getPageTemplateList();
			setValueList(0, "template", s);
			if(s.length > 0) {
				//take from preferences
				String defaultPageTemplate = getDefaultPageTemplate();
				if (defaultPageTemplate == null) {
					defaultPageTemplate = s[0];
				} else {
					boolean bFound = false;
					for (int i = 0; i < s.length && !bFound; i++) {
						if (s[i].equals(defaultPageTemplate)) bFound = true; 
					}
					if (!bFound) defaultPageTemplate = s[0];
				}
				setAttributeValue(0, "template", defaultPageTemplate);
			}
		}
		targetHolder.target = getTarget();
		IResource r = (IResource)getTarget().getAdapter(IResource.class);
		if(r == null) {
			setAttributeValue(0, "folder", "");
			targetHolder.revalidate(null);
		} else {
			targetHolder.revalidate(r.getFullPath().toString());
			setAttributeValue(0, "folder", "" + targetHolder.path);
		}
		initVersions();
	}
	
	protected String getDefaultPageTemplate() {
		return null;
	}
	void initVersions() {
		useVersions = false;
		versionEntities.clear();
		XModelMetaData meta = getTarget().getModel().getMetaData();
		XMapping m = meta.getMapping("FileVersions");
		if(m == null) return;
		String entityVersion = action.getProperty("entityVersion");
		if(entityVersion == null || entityVersion.length() == 0) return;
		if(getAttributeValue(0, "version") == null) return;
		useVersions = true;
		String[] keys = m.getKeys();
		for (int i = 0; i < keys.length; i++) {
			if(!keys[i].startsWith(entityVersion)) continue;
			String entity = m.getValue(keys[i]);
			if(meta.getEntity(entity) == null) continue;
			String version = keys[i].substring(entityVersion.length());
			versionEntities.put(version, entity);
		}
		if(versionEntities.size() == 0 && action.getProperty("entity") != null) {
			versionEntities.put("default", action.getProperty("entity"));
		}
		String[] versionList = (String[])versionEntities.keySet().toArray(new String[0]);
		setValueList(0, "version", versionList);
		if(versionList.length > 0) {
			setAttributeValue(0, "version", versionList[0]);
		}
	}
	
	private boolean hasTemplate() {
		return findAttribute(0, "template") != null; 
	}

	public void action(String name) throws Exception {
		if(FINISH.equals(name)) {
			if(!checkResource()) return;
			execute();
			setFinished(true);
		} else if(CANCEL.equals(name)) {
			setFinished(true);
		} else if(HELP.equals(name)) {
			help();
		}
	}

	public String[] getActionNames(int stepId) {
		return new String[]{FINISH, CANCEL, HELP};
	}

	protected void execute() throws Exception {
		Properties p = extractStepData(0);
		String path = p.getProperty("name");
		path = revalidatePath(path);
		XModelObject f = createFile(path);
		if(f != null) targetHolder.saveLastPath();
		if(f != null) open(f);		
	}
	
	protected boolean checkResource() {
		if(targetHolder.addPath.length() == 0) return true;
		ServiceDialog d = getTarget().getModel().getService();
		String message = "Folder " + targetHolder.path + " does not exist. Do you want to create it?";
		int q = d.showDialog("Warning", message, new String[]{SpecialWizardSupport.OK, SpecialWizardSupport.CANCEL}, null, ServiceDialog.QUESTION);
		return q == 0;
	}
	
	public boolean isFieldEditorEnabled(int stepId, String name, Properties values) {
		String path = values.getProperty("name");
		boolean c = canCreateFile(path);
		if(name.equals("template")) {
			return c;
		}
		return true;
	}
	
	public boolean canCreateFile(String path) {
		if(targetHolder.target == null) return false;
		if(path == null || path.length() == 0 || path.indexOf('*') >= 0) return false;
		return isCorrectPath(path) && !fileExists(path);
	}
	
	boolean isCorrectPath(String path) {
		path = revalidatePath(path);
		if(path == null || path.equals("/") || path.indexOf("//") >= 0) return false;
		  return true;
	}
	
	boolean fileExists(String path) {
		if(path == null || targetHolder.target == null) return false;
		path = revalidatePath(path);
		if(path.startsWith("/")) path = path.substring(1);
		return targetHolder.target.getChildByPath(path) != null;
	} 
	
	protected String revalidatePath(String path) {
		if(path == null || path.length() == 0) return path;
		if(!path.startsWith("/")) path = "/" + path;
		String extension = "." + action.getProperty("extension");
		if(path.lastIndexOf('.') < 0) {
			path += extension;
		} else {
			String ext = path.substring(path.lastIndexOf('.'));
			if(!ext.equals(extension) && extension.length() > 1) path += extension;
		}
		if(targetHolder.addPath.length() > 0) {
			path = targetHolder.addPath + path;
		}
		return path;
	}

	protected XModelObject createFile(String path) throws Exception {
		if(!canCreateFile(path)) return null;
		String body = getTemplateBody();
		body = modifyBody(body);
		return createFile(targetHolder.target, path, body, extractStepData(0));
	}
	
	protected String getTemplateBody() throws Exception {
		File templateFile = null;
		String template = getAttributeValue(0, "template");
		if(template != null && template.trim().length() > 0) {
			templateFile = findTemplate(template.trim());
			if(templateFile == null || !templateFile.isFile()) throw new Exception("Template " + template + " is not found.");
		}		
		return (templateFile == null) ? "" : FileUtil.readFile(templateFile);
	}
	
	protected String modifyBody(String body) {
		return body;
	}
	
	protected void open(final XModelObject f) {
		Display.getDefault().asyncExec(new Runnable() {
			public void run() {        
				FindObjectHelper.findModelObject(f, FindObjectHelper.EVERY_WHERE);
			}
		});
	} 

	XModelObject createFile(XModelObject fs, String path, String body, Properties p) {
		StringTokenizer st = new StringTokenizer(path, "/");
		int c = st.countTokens(), i = 0;
		while(i < c - 1) {
			String s = st.nextToken();
			XModelObject o = fs.getChildByPath(s);
			if(o == null) {
				o = fs.getModel().createModelObject("FileFolder", null);
				o.setAttributeValue("name", s);
				DefaultCreateHandler.addCreatedObject(fs, o, getProperties());
				((FolderImpl)o).save();
			}
			fs = o;
			i++;
		}
		String s = st.nextToken();
		int dot = s.lastIndexOf('.');
		String n = s.substring(0, dot);
		String e = s.substring(dot + 1);
		String entity = getFileEntity(e);
		XModelObject f = XModelObjectLoaderUtil.createValidObject(fs.getModel(), entity, p);
		f.setAttributeValue("name", n);
		f.setAttributeValue("extension", e);
		if(body != null) f.setAttributeValue("body", body);
		f = modifyCreatedObject(f);
		XModelObject fq = fs.getChildByPath(f.getPathPart());
		if(fq != null) return fq;
		DefaultCreateHandler.addCreatedObject(fs, f, getProperties());
		if(f instanceof FileAnyImpl && f.getModelEntity().getAttribute(XModelObjectConstants.ATTR_NAME_ENCODING) != null) {
			String txt = ((FileAnyImpl)f).getAsText();
			String enc = XModelObjectLoaderUtil.getEncoding(txt);
			if(enc != null) f.setAttributeValue(XModelObjectConstants.ATTR_NAME_ENCODING, enc);
		}
		String modelPath = f.getPath();
		((FolderImpl)fs).saveChild(f);
		fs.getModel().update();
		if(f.getParent() == null) {
			//entity could change after saving child.
			f = f.getModel().getByPath(modelPath);
		}
		if(f != null) getProperties().put("created", f);
		return f;
	}
	
	protected String getFileEntity(String extension) {
		if(useVersions) {
			String version = getAttributeValue(0, "version");
			String entity = version == null ? null : (String)versionEntities.get(version);
			if(entity != null) return entity;
		}
		return ("jsp".equals(extension)) ? "FileJSP" :
			("htm".equals(extension)) ? "FileHTML" :
			("html".equals(extension)) ? "FileHTML" :
			("properties".equals(extension)) ? "FilePROPERTIES" :
			(extension.equals(action.getProperty("extension"))) ? action.getProperty("entity") :
			"FileAny";
	}

	protected XModelObject modifyCreatedObject(XModelObject o) {
		return o;
	}
	
	protected File findTemplate(String name) {
		return null;
	}
	
	public String[] getPageTemplateList() {
		return new String[0];
	}
	
	protected final XModelObject getTargetFolder() {
		return targetHolder.target;
	}
	
	class TargetHolder {
		XModelObject target;
		String path;
		IResource folder;
		String addPath = "";
		
		public void revalidate(String newPath) {
			if(newPath == path || (newPath != null && newPath.equals(path))) return;
			path = newPath;
			addPath = "";
			folder = (path == null) ? null : ModelPlugin.getWorkspace().getRoot().findMember(path);
			target = EclipseResourceUtil.getObjectByResource(folder);
			if(path != null && (folder == null || !folder.exists())) {
				String p = path.replace('\\', '/');
				String ap = "";
				while(true) {
					int q = p.lastIndexOf('/');
					if(q < 0) break;
					ap = p.substring(q) + ap;
					p = p.substring(0, q);
					folder = ModelPlugin.getWorkspace().getRoot().findMember(p);
					if(folder != null && folder.exists()) {
						addPath = ap;
						if(addPath.endsWith("/")) addPath = addPath.substring(0, addPath.length() - 1);
						target = EclipseResourceUtil.createObjectForResource(folder);
						break;
					}
				}
			} else if(target == null && folder != null && folder.exists()) {
				target = EclipseResourceUtil.createObjectForResource(folder);
			} else if(target != null && "FileSystems".equals(target.getModelEntity().getName())) {
				target = EclipseResourceUtil.findFileSystem(folder, target.getModel());
				if(target == null) target = EclipseResourceUtil.createObjectForResource(folder);
			}
		}
		public void saveLastPath() {
			if(path == null || folder == null) return;
			QualifiedName n = new QualifiedName("", action.getName() + "_lastPath");
			try {
				folder.getProject().setPersistentProperty(n, path);
			} catch (Exception e) {
				ModelPlugin.getPluginLog().logError("CreateFileSuppport:TargetHolder:saveLastPath:" + e.getMessage());
			}
		}
		
	}

	protected DefaultWizardDataValidator validator = createValidator();
    
	public WizardDataValidator getValidator(int step) {
		validator.setSupport(this, step);
		return validator;    	
	}
	
	protected DefaultWizardDataValidator createValidator() {
		return new Validator(); 
	}
	
	protected class Validator extends DefaultWizardDataValidator {
		public void validate(Properties data) {
			String folder = data.getProperty("folder");
			targetHolder.revalidate(folder);
			message = null;
			validateFolderName();
			if(message != null) return;
			validateFileName(data);
			if(message != null) return;
			String template = data.getProperty("template");
			if(template != null && template.trim().length() > 0) {
				File templateFile = findTemplate(template.trim());
				if(templateFile == null || !templateFile.isFile()) {
					message = "Template does not exist.";
				}
			}
			if(message != null) return;
			super.validate(data);
		}
		
		String FORBIDDEN_INDICES = "\"\n\t*\\/:<>?|";
		protected void validateFileName(Properties data) {
			if(message != null) return;
			String fileName = data.getProperty("name");
			if(fileName == null || fileName.length() == 0) return;
			if(fileName.equals(".")) {
				message = "Incorrect file name.";
			} else if(fileName.endsWith(".") && fileName.indexOf('.') != fileName.lastIndexOf('.')) {
				message = "File name must not end in a period.";
			} else {
				for (int i = 0; i < FORBIDDEN_INDICES.length(); i++) {
					if(fileName.indexOf(FORBIDDEN_INDICES.charAt(i)) >= 0) {
						message = "File name must not contain character " + FORBIDDEN_INDICES.charAt(i) + " .";
						return;
					}
				}				
			}
		}

		String FORBIDDEN_FOLDER_LETTERS = "\"\n\t*:<>?|";
		private void validateFolderName() {
			if(targetHolder.addPath.length() == 0) return;
			for (int i = 0; i < FORBIDDEN_FOLDER_LETTERS.length(); i++) {
				if(targetHolder.addPath.indexOf(FORBIDDEN_FOLDER_LETTERS.charAt(i)) >= 0) {
					message = "Folder name must not contain character " + FORBIDDEN_FOLDER_LETTERS.charAt(i) + " .";
					return;
				}
			}				
		}
		

		protected void validateAddFile(XEntityData[] ds, Properties data) {
			CreateFileHandler.validateNameAndExtension(action, data, null);
			if(targetHolder.target != null) {
				String entity = action.getProperty("entity");
				if(entity == null) entity = getEntityData()[step].getModelEntity().getName();
				if(targetHolder.addPath == null || targetHolder.addPath.length() == 0) {
					if(!checkChild(targetHolder.target, entity, data)) return;
				}
			} else {
				message = "Cannot create file in specified folder.";
			}
		}
	
	}

	public String getFocusAttribute(int stepId) {
		if(stepId == 0) {
			return "name";
		}
		return super.getFocusAttribute(stepId);
	}

}
