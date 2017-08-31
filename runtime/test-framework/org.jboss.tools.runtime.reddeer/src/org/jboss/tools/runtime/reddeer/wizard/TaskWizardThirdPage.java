package org.jboss.tools.runtime.reddeer.wizard;


import org.eclipse.reddeer.core.reference.ReferencedComposite;
import org.eclipse.reddeer.jface.wizard.WizardPage;
import org.eclipse.reddeer.swt.impl.button.CheckBox;
import org.eclipse.reddeer.swt.impl.text.LabeledText;

public class TaskWizardThirdPage extends WizardPage{
	
	public TaskWizardThirdPage(ReferencedComposite referencedComposite) {
		super(referencedComposite);
	}

	public void setInstallFolder(String path){
		new LabeledText(referencedComposite, "Install folder:").setText(path);
	}
	
	public void setDownloadFolder(String path){
		new LabeledText(referencedComposite, "Download folder:").setText(path);
	}
	
	public void setDeleteArchive(boolean delete){
		if(delete && !new CheckBox(referencedComposite, "Delete archive after installing").isChecked()){
			new CheckBox(referencedComposite, "Delete archive after installing").click();
		}
	}

}
