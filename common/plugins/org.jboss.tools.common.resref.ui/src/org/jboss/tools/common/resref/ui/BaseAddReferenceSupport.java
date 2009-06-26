package org.jboss.tools.common.resref.ui;

import java.util.Properties;

import org.eclipse.core.resources.IFile;
import org.jboss.tools.common.meta.action.XActionInvoker;
import org.jboss.tools.common.meta.action.impl.SpecialWizardSupport;
import org.jboss.tools.common.meta.constraint.XAttributeConstraintL;
import org.jboss.tools.common.model.XModel;
import org.jboss.tools.common.model.XModelException;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.options.PreferenceModelUtilities;
import org.jboss.tools.common.resref.core.ResourceReference;

public abstract class BaseAddReferenceSupport extends SpecialWizardSupport {

	public static boolean add(IFile file, ResourceReference css, ResourceReference[] list, String entity) {
		return run(file, css, list, "CreateActions.AddItem", entity); //$NON-NLS-1$
	}

	public static boolean edit(IFile file, ResourceReference css, ResourceReference[] list,
			String entity) {
				return run(file, css, list, "EditActions.EditItem", entity); //$NON-NLS-1$
			}

	private static boolean run(IFile file, ResourceReference css, ResourceReference[] list,
			String action, String entity) {
				XModel model = PreferenceModelUtilities.getPreferenceModel();
				XModelObject object = model.createModelObject(entity, null);
				object.setAttributeValue("location", css.getLocation()); //$NON-NLS-1$
				if(object.getAttributeValue("prefix") != null) { //$NON-NLS-1$
					object.setAttributeValue("prefix", css.getProperties()); //$NON-NLS-1$
				}
				Properties p = new Properties();
				p.put("resourceReference", css); //$NON-NLS-1$
				p.put("scope",Integer.valueOf(css.getScope())); //$NON-NLS-1$
				p.put("list", list); //$NON-NLS-1$
				if(file != null) p.put("file", file); //$NON-NLS-1$
				XActionInvoker.invoke(action, object, p);
				boolean ok = "true".equals(p.getProperty("okPressed")); //$NON-NLS-1$ //$NON-NLS-2$
				if(ok) {
					css.setLocation(object.getAttributeValue("location")); //$NON-NLS-1$
					Integer scope = (Integer)p.get("scope"); //$NON-NLS-1$
					
					css.setScope(scope.intValue());
					if(css.isGlobal()){
					    css.setScope(ResourceReference.GLOBAL_SCOPE);
					}
					String properties = object.getAttributeValue("prefix"); //$NON-NLS-1$
					if(properties != null) css.setProperties(properties);
				}
				return ok;
			}

	protected IFile file = null;
	String initialLocation;
	String initialPrefix;
	ResourceReference[] list;
	String[] scopeNames;

	public BaseAddReferenceSupport() {
		super();
	}

	protected void reset() {
		initialLocation = getTarget().getAttributeValue("location"); //$NON-NLS-1$
		setAttributeValue(0, "location", initialLocation); //$NON-NLS-1$
		initialPrefix = getTarget().getAttributeValue("prefix"); //$NON-NLS-1$
		if(initialPrefix != null) {
			setAttributeValue(0, "prefix", initialPrefix); //$NON-NLS-1$
		}
		if(getTarget().getModelEntity().getAttribute("scope") != null) {  //$NON-NLS-1$
			final XAttributeConstraintL scopeAttribute = ((XAttributeConstraintL) getTarget().getModelEntity().getAttribute("scope") //$NON-NLS-1$
	            .getConstraint());
			if (scopeAttribute != null) {
				scopeNames = scopeAttribute.getValues();
			}
		} else {
			//just in case. should not happen
			scopeNames = ResourceReference.SCOPE_NAMES;
		}
		int scopeIndex = ((Integer)getProperties().get("scope")).intValue(); //$NON-NLS-1$
		
		if(scopeIndex == 1 && scopeNames.length == 1){
		    scopeIndex = 0;
		}else if(scopeIndex > scopeNames.length){
		    scopeIndex = scopeNames.length -1;
		}
		String scope = scopeNames[scopeIndex];
		setAttributeValue(0, "scope", scope); //$NON-NLS-1$
		list = (ResourceReference[])getProperties().get("list"); //$NON-NLS-1$
		file = (IFile)getProperties().get("file"); //$NON-NLS-1$
	}

	public void action(String name) throws XModelException {
		if(OK.equals(name) || FINISH.equals(name)) {
			execute();
			setFinished(true);
			getProperties().setProperty("okPressed", "true"); //$NON-NLS-1$ //$NON-NLS-2$
		} else if(CANCEL.equals(name)) {
			setFinished(true);
		}
	}

	protected void execute() throws XModelException {
		Properties p0 = extractStepData(0);
		getTarget().setAttributeValue("location", p0.getProperty("location")); //$NON-NLS-1$ //$NON-NLS-2$
		if(p0.containsKey("prefix")) { //$NON-NLS-1$
			getTarget().setAttributeValue("prefix", p0.getProperty("prefix")); //$NON-NLS-1$ //$NON-NLS-2$
		}
		int scope = getSelectedScope(p0); 
		getProperties().put("scope", Integer.valueOf(scope)); //$NON-NLS-1$
	}

	public int getSelectedScope(Properties p0) {
		String scopeName = p0.getProperty("scope"); //$NON-NLS-1$
		for (int i = 0; i < scopeNames.length; i++) {
			if(scopeNames[i].equals(scopeName)) return i;
		}
		return 0;
	}

}