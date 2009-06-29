package org.jboss.tools.common.model.files.handlers;

import java.text.MessageFormat;
import java.util.Properties;
import java.util.StringTokenizer;

import org.eclipse.core.resources.IResource;
import org.jboss.tools.common.meta.action.XEntityData;
import org.jboss.tools.common.meta.action.impl.DefaultWizardDataValidator;
import org.jboss.tools.common.meta.action.impl.SpecialWizardSupport;
import org.jboss.tools.common.meta.action.impl.WizardDataValidator;
import org.jboss.tools.common.meta.action.impl.handlers.DefaultCreateHandler;
import org.jboss.tools.common.meta.impl.XMetaDataConstants;
import org.jboss.tools.common.model.ServiceDialog;
import org.jboss.tools.common.model.XModelException;
import org.jboss.tools.common.model.XModelObjectConstants;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.filesystems.impl.CreateFileHandler;
import org.jboss.tools.common.model.filesystems.impl.FolderImpl;
import org.jboss.tools.common.model.plugin.ModelMessages;

public class CreateFolderSupport extends SpecialWizardSupport {
	protected TargetHolder targetHolder = new TargetHolder();
	
	public CreateFolderSupport() {}

	public void reset() {
		targetHolder.setAction(action);
		targetHolder.target = getTarget();
		IResource r = (IResource)getTarget().getAdapter(IResource.class);
		if(r == null) {
			setAttributeValue(0, "folder", ""); //$NON-NLS-1$ //$NON-NLS-2$
			targetHolder.revalidate(null);
		} else {
			targetHolder.revalidate(r.getFullPath().toString());
			setAttributeValue(0, "folder", "" + targetHolder.path); //$NON-NLS-1$ //$NON-NLS-2$
		}
	}

	@Override
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

	protected boolean checkResource() {
		if(targetHolder.addPath.length() == 0) return true;
		ServiceDialog d = getTarget().getModel().getService();
		String message = MessageFormat.format(
				"Folder {0} does not exist. Do you want to create it?",
				targetHolder.path);
		int q = d.showDialog(ModelMessages.WARNING, message, new String[]{SpecialWizardSupport.OK, SpecialWizardSupport.CANCEL}, null, ServiceDialog.QUESTION);
		return q == 0;
	}
	
	public boolean canCreateResource(String path) {
		if(targetHolder.target == null) return false;
		if(path == null || path.length() == 0 || path.indexOf('*') >= 0) return false;
		return isCorrectPath(path) && !resourceExists(path);
	}
	
	boolean isCorrectPath(String path) {
		path = revalidatePath(path);
		if(path == null || path.equals(XModelObjectConstants.SEPARATOR) || path.indexOf("//") >= 0) return false; //$NON-NLS-1$
		  return true;
	}
	
	boolean resourceExists(String path) {
		if(path == null || targetHolder.target == null) return false;
		path = revalidatePath(path);
		if(path.startsWith(XModelObjectConstants.SEPARATOR)) path = path.substring(1);
		return targetHolder.target.getChildByPath(path) != null;
	} 
	
	protected String revalidatePath(String path) {
		if(path == null || path.length() == 0) return path;
		if(!path.startsWith(XModelObjectConstants.SEPARATOR)) path = XModelObjectConstants.SEPARATOR + path;
		if(targetHolder.addPath.length() > 0) {
			path = targetHolder.addPath + path;
		}
		return path;
	}

	protected void execute() throws XModelException {
		Properties p = extractStepData(0);
		String path = p.getProperty(XModelObjectConstants.ATTR_NAME);
		path = revalidatePath(path);
		XModelObject f = createFolder(path);
		if(f != null) targetHolder.saveLastPath();
	}
	
	protected XModelObject createFolder(String path) throws XModelException {
		if(!canCreateResource(path)) return null;
		XModelObject fs = targetHolder.target;

		StringTokenizer st = new StringTokenizer(path, XModelObjectConstants.SEPARATOR);
		int c = st.countTokens(), i = 0;
		while(i < c) {
			String s = st.nextToken();
			XModelObject o = fs.getChildByPath(s);
			if(o == null) {
				o = fs.getModel().createModelObject("FileFolder", null); //$NON-NLS-1$
				o.setAttributeValue(XModelObjectConstants.ATTR_NAME, s);
				DefaultCreateHandler.addCreatedObject(fs, o, getProperties());
				((FolderImpl)o).save();
			}
			fs = o;
			i++;
		}

		return fs;
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
			String folder = data.getProperty("folder"); //$NON-NLS-1$
			targetHolder.revalidate(folder);
			message = null;
			if(targetHolder.target == null) {
				message = "Cannot create folder in specified folder.";
				return;
			}
			validateFolderName();
			if(message != null) return;
			validateChildName(data);
			if(message != null) return;
			super.validate(data);
		}
		
		String FORBIDDEN_INDICES = "\"\n\t*\\/:<>?|"; //$NON-NLS-1$
		protected void validateChildName(Properties data) {
			if(message != null) return;
			String name = data.getProperty(XModelObjectConstants.ATTR_NAME);
			if(name == null || name.length() == 0) return;
			if(name.equals(".")) { //$NON-NLS-1$
				message = "Incorrect name.";
			} else if(name.endsWith(".") && name.indexOf('.') != name.lastIndexOf('.')) { //$NON-NLS-1$
				message = "Name must not end in a period.";
			} else {
				for (int i = 0; i < FORBIDDEN_INDICES.length(); i++) {
					if(name.indexOf(FORBIDDEN_INDICES.charAt(i)) >= 0) {
						message = MessageFormat.format(
								"Name must not contain character {0} .",
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
