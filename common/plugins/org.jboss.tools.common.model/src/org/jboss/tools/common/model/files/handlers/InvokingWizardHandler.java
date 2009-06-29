package org.jboss.tools.common.model.files.handlers;

import java.util.Properties;

import org.jboss.tools.common.meta.action.SpecialWizard;
import org.jboss.tools.common.meta.action.SpecialWizardFactory;
import org.jboss.tools.common.meta.action.XActionInvoker;
import org.jboss.tools.common.meta.action.impl.AbstractHandler;
import org.jboss.tools.common.model.XModelException;
import org.jboss.tools.common.model.XModelObject;

public class InvokingWizardHandler extends AbstractHandler {
	SpecialWizard sw = SpecialWizardFactory.createSpecialWizard("org.jboss.tools.common.model.ui.wizard.newfile.WizardInvoker"); //$NON-NLS-1$
	
	public InvokingWizardHandler() {}
	
    public boolean isEnabled(XModelObject object) {
        return sw != null && object != null;
    }

    public void executeHandler(XModelObject object, Properties p) throws XModelException {
    	if(sw == null || object == null) return;
    	if(p == null) p = new Properties();
		String pluginId = action.getProperty("plugin"); //$NON-NLS-1$
		String wizardId = action.getProperty("wizard"); //$NON-NLS-1$

		p.setProperty("plugin", pluginId); //$NON-NLS-1$
		p.setProperty("wizard", wizardId); //$NON-NLS-1$
		p.put("object", object); //$NON-NLS-1$

    	sw.setObject(p);
    	int r = sw.execute();
    	// Returns 1 if wizard was not found.
    	if(r == 1) {
    		String path = getTruePath();
        	if(path != null) XActionInvoker.invoke(path, object, p);
    	}
    }
    
    private String getTruePath() {
    	String path = action.getProperty("action"); //$NON-NLS-1$
    	if(path != null) return path;
    	path = action.getPath();
		if(path.endsWith("V")) { //$NON-NLS-1$
			path = path.substring(0, path.length() - 1).replace('/', '.');
		} else {
			path = null;
		}
		return path;
    }

}
