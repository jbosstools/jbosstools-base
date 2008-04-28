package org.jboss.tools.common.model.ui.wizard.newfile;

import java.util.Properties;

import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.PlatformUI;
import org.jboss.tools.common.meta.action.SpecialWizard;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.ui.ModelUIPlugin;
import org.jboss.tools.common.model.ui.util.ExtensionPointUtils;

public class WizardInvoker implements SpecialWizard {
	Properties p;

	public void setObject(Object object) {
		if(object instanceof Properties) {
			p = (Properties)object;
		}
		
	}

	public int execute() {
		if(p == null) {
			return 1;
		}
		String pluginId = p.getProperty("plugin");
		String wizardId = p.getProperty("wizard");
		XModelObject s = (XModelObject)p.get("object");
		INewWizard wizard = ExtensionPointUtils.findNewWizardsItem(pluginId, wizardId);
		
		if(wizard == null || s == null) {
			return 1;
		}

		wizard.init(PlatformUI.getWorkbench(), new StructuredSelection(s));
		
		WizardDialog dialog = new WizardDialog(ModelUIPlugin.getDefault().getWorkbench().getActiveWorkbenchWindow().getShell(), wizard);
		dialog.create();
		PlatformUI.getWorkbench().getHelpSystem().setHelp(dialog.getShell(), "org.eclipse.ui.new_wizard_shortcut_context");
		dialog.open();  

		return 0;
	}

}
