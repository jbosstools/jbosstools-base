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
import java.text.MessageFormat;
import java.util.*;

import org.eclipse.core.resources.IResource;
import org.eclipse.swt.widgets.Display;

import org.jboss.tools.common.meta.XMapping;
import org.jboss.tools.common.meta.XModelMetaData;
import org.jboss.tools.common.meta.action.*;
import org.jboss.tools.common.meta.action.impl.*;
import org.jboss.tools.common.meta.action.impl.handlers.DefaultCreateHandler;
import org.jboss.tools.common.meta.impl.XMetaDataConstants;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.filesystems.impl.*;
import org.jboss.tools.common.model.loaders.impl.MappedEntityRecognizer;
import org.jboss.tools.common.model.plugin.ModelMessages;
import org.jboss.tools.common.model.util.*;
import org.jboss.tools.common.util.FileUtil;

public class CreateFileSupport extends SpecialWizardSupport {
	public static String INITIAL_FOLDER_PROPERTY = "initialFolder"; //$NON-NLS-1$
	public static String INITIAL_FILE_PROPERTY = "initialFile"; //$NON-NLS-1$
	
	static final String ATTR_TEMPLATE = "template"; //$NON-NLS-1$
	static final String ATTR_FOLDER = "folder"; //$NON-NLS-1$

	protected TargetHolder targetHolder = new TargetHolder();
	TreeMap<String,String> versionEntities = new TreeMap<String,String>();
	boolean useVersions = false;
	
	public void reset() {
		targetHolder.setAction(action);
		if(hasTemplate()) {
			String[] s = getPageTemplateList();
			setValueList(0, ATTR_TEMPLATE, s);
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
				setAttributeValue(0, ATTR_TEMPLATE, defaultPageTemplate);
			}
		}
		targetHolder.target = getTarget();
		IResource r = (IResource)getTarget().getAdapter(IResource.class);
		if(r == null) {
			setAttributeValue(0, ATTR_FOLDER, ""); //$NON-NLS-1$
			targetHolder.revalidate(null);
		} else {
			targetHolder.revalidate(r.getFullPath().toString());
			setAttributeValue(0, ATTR_FOLDER, "" + targetHolder.path); //$NON-NLS-1$
		}
		String initialFolder = p.getProperty(INITIAL_FOLDER_PROPERTY);
		if(initialFolder != null && initialFolder.length() > 0) {
			setAttributeValue(0, ATTR_FOLDER, initialFolder);
		}
		String initialFile = p.getProperty(INITIAL_FILE_PROPERTY);
		if(initialFile != null && initialFile.length() > 0) {
			setAttributeValue(0, XModelObjectConstants.ATTR_NAME, initialFile);
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
		XMapping m = meta.getMapping("FileVersions"); //$NON-NLS-1$
		if(m == null) return;
		String entityVersion = action.getProperty("entityVersion"); //$NON-NLS-1$
		if(entityVersion == null || entityVersion.length() == 0) return;
		if(getAttributeValue(0, "version") == null) return; //$NON-NLS-1$
		useVersions = true;
		String[] keys = m.getKeys();
		for (int i = 0; i < keys.length; i++) {
			if(!keys[i].startsWith(entityVersion)) continue;
			String entity = m.getValue(keys[i]);
			if(meta.getEntity(entity) == null) continue;
			String version = keys[i].substring(entityVersion.length());
			versionEntities.put(version, entity);
		}
		if(versionEntities.size() == 0 && action.getProperty(XMetaDataConstants.ENTITY) != null) {
			versionEntities.put("default", action.getProperty(XMetaDataConstants.ENTITY)); //$NON-NLS-1$
		}
		String[] versionList = (String[])versionEntities.keySet().toArray(new String[0]);
		setValueList(0, "version", versionList); //$NON-NLS-1$
		if(versionList.length > 0) {
			setAttributeValue(0, "version", versionList[0]); //$NON-NLS-1$
		}
	}
	
	private boolean hasTemplate() {
		return findAttribute(0, ATTR_TEMPLATE) != null; 
	}

	public void action(String name) throws XModelException {
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

	protected void execute() throws XModelException {
		Properties p = extractStepData(0);
		String path = p.getProperty(XModelObjectConstants.ATTR_NAME);
		path = revalidatePath(path);
		XModelObject f = createFile(path);
		if(f != null) targetHolder.saveLastPath();
		if(f != null) open(f);		
	}
	
	protected boolean checkResource() {
		if(targetHolder.addPath.length() == 0) return true;
		ServiceDialog d = getTarget().getModel().getService();
		String message = MessageFormat.format(
				"Folder {0} does not exist. Do you want to create it?",
				targetHolder.path);
		int q = d.showDialog(ModelMessages.WARNING, message, new String[]{SpecialWizardSupport.OK, SpecialWizardSupport.CANCEL}, null, ServiceDialog.QUESTION);
		return q == 0;
	}
	
	public boolean isFieldEditorEnabled(int stepId, String name, Properties values) {
		String path = values.getProperty(XModelObjectConstants.ATTR_NAME);
		boolean c = canCreateFile(path);
		if(name.equals(ATTR_TEMPLATE)) {
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
		if(path == null || path.equals(XModelObjectConstants.SEPARATOR) || path.indexOf("//") >= 0) return false; //$NON-NLS-1$
		  return true;
	}
	
	boolean fileExists(String path) {
		if(path == null || targetHolder.target == null) return false;
		path = revalidatePath(path);
		if(path.startsWith(XModelObjectConstants.SEPARATOR)) path = path.substring(1);
		return targetHolder.target.getChildByPath(path) != null;
	} 
	
	protected String revalidatePath(String path) {
		if(path == null || path.length() == 0) return path;
		if(!path.startsWith(XModelObjectConstants.SEPARATOR)) path = XModelObjectConstants.SEPARATOR + path;
		String extension = "." + action.getProperty(XModelObjectConstants.ATTR_NAME_EXTENSION); //$NON-NLS-1$
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

	protected XModelObject createFile(String path) throws XModelException {
		if(!canCreateFile(path)) return null;
		try {
			String body = getTemplateBody();
			body = modifyBody(body);
			return createFile(targetHolder.target, path, body, extractStepData(0));
		} catch (IOException e) {
			throw new XModelException(e);
		}
	}
	
	protected String getTemplateBody() throws IOException {
		File templateFile = null;
		String template = getAttributeValue(0, ATTR_TEMPLATE);
		if(template != null && template.trim().length() > 0) {
			templateFile = findTemplate(template.trim());
			if(templateFile == null || !templateFile.isFile()) throw new IOException(
					MessageFormat.format("Template {0} is not found.", template));
		}		
		return (templateFile == null) ? "" : FileUtil.readFile(templateFile); //$NON-NLS-1$
	}
	
	protected String modifyBody(String body) throws IOException {
		return body;
	}
	
	protected void open(final XModelObject f) {
		Display.getDefault().asyncExec(new Runnable() {
			public void run() {        
				FindObjectHelper.findModelObject(f, FindObjectHelper.EVERY_WHERE);
			}
		});
	} 

	XModelObject createFile(XModelObject fs, String path, String body, Properties p) throws XModelException {
		StringTokenizer st = new StringTokenizer(path, XModelObjectConstants.SEPARATOR);
		int c = st.countTokens(), i = 0;
		while(i < c - 1) {
			String s = st.nextToken();
			XModelObject o = fs.getChildByPath(s);
			if(o == null) {
				o = fs.getModel().createModelObject("FileFolder", null); //$NON-NLS-1$
				o.setAttributeValue(XModelObjectConstants.ATTR_NAME, s);
				DefaultCreateHandler.addCreatedObject(fs, o, FindObjectHelper.IN_NAVIGATOR_ONLY);
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
		f.setAttributeValue(XModelObjectConstants.ATTR_NAME, n);
		f.setAttributeValue(XModelObjectConstants.ATTR_NAME_EXTENSION, e);
		if(body != null) f.setAttributeValue(XModelObjectConstants.ATTR_NAME_BODY, body);
		f = modifyCreatedObject(f);
		XModelObject fq = fs.getChildByPath(f.getPathPart());
		if(fq != null) return fq;
		DefaultCreateHandler.addCreatedObject(fs, f, FindObjectHelper.IN_NAVIGATOR_ONLY);
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
		if(f != null) getProperties().put("created", f); //$NON-NLS-1$
		return f;
	}
	
	protected String getFileEntity(String extension) {
		if(useVersions) {
			String version = getAttributeValue(0, "version"); //$NON-NLS-1$
			String entity = version == null ? null : (String)versionEntities.get(version);
			if(entity != null) return entity;
		}
		
		String entity = null;
		if(extension.equals(action.getProperty(XModelObjectConstants.ATTR_NAME_EXTENSION))) {
			entity = action.getProperty(XMetaDataConstants.ENTITY);
		}
		if(entity == null) {
			entity = new MappedEntityRecognizer().getEntityName(extension, null);
		}
		if(entity != null && getTarget().getModel().getMetaData().getEntity(entity) != null) {
			return entity;
		}
		
		return "FileAny"; //$NON-NLS-1$
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
			String folder = data.getProperty(ATTR_FOLDER);
			targetHolder.revalidate(folder);
			message = null;
			validateFolderName();
			if(message != null) return;
			validateFileName(data);
			if(message != null) return;
			String template = data.getProperty(ATTR_TEMPLATE);
			if(template != null && template.trim().length() > 0) {
				File templateFile = findTemplate(template.trim());
				if(templateFile == null || !templateFile.isFile()) {
					message = "Template does not exist.";
				}
			}
			if(message != null) return;
			super.validate(data);
		}
		
		String FORBIDDEN_INDICES = "\"\n\t*\\/:<>?|"; //$NON-NLS-1$
		protected void validateFileName(Properties data) {
			if(message != null) return;
			String fileName = data.getProperty(XModelObjectConstants.ATTR_NAME);
			if(fileName == null || fileName.length() == 0) return;
			if(fileName.equals(".")) { //$NON-NLS-1$
				message = "Incorrect file name.";
			} else if(fileName.endsWith(".") && fileName.indexOf('.') != fileName.lastIndexOf('.')) { //$NON-NLS-1$
				message = "File name must not end in a period.";
			} else {
				for (int i = 0; i < FORBIDDEN_INDICES.length(); i++) {
					if(fileName.indexOf(FORBIDDEN_INDICES.charAt(i)) >= 0) {
						message = MessageFormat.format(
								"File name must not contain character {0} .",
								FORBIDDEN_INDICES.charAt(i));
						return;
					}
				}				
			}
		}

		String FORBIDDEN_FOLDER_LETTERS = "\"\n\t*:<>?|"; //$NON-NLS-1$
		private void validateFolderName() {
			if(targetHolder.addPath.length() == 0) return;
			for (int i = 0; i < FORBIDDEN_FOLDER_LETTERS.length(); i++) {
				if(targetHolder.addPath.indexOf(FORBIDDEN_FOLDER_LETTERS.charAt(i)) >= 0) {
					message = MessageFormat.format(
							"Folder name must not contain character {0} .",
							FORBIDDEN_FOLDER_LETTERS.charAt(i));
					return;
				}
			}				
		}
		

		protected void validateAddFile(XEntityData[] ds, Properties data) {
			CreateFileHandler.validateNameAndExtension(action, data, null);
			if(targetHolder.target != null) {
				String entity = action.getProperty(XMetaDataConstants.ENTITY);
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
			return XModelObjectConstants.ATTR_NAME;
		}
		return super.getFocusAttribute(stepId);
	}

}
