package org.jboss.tools.runtime.reddeer.wizard;


import org.eclipse.reddeer.jface.wizard.WizardPage;
import org.eclipse.reddeer.swt.impl.button.CheckBox;
import org.eclipse.reddeer.swt.impl.text.LabeledText;

public class TaskWizardThirdPage extends WizardPage{
	
	public void setInstallFolder(String path){
		new LabeledText("Install folder:").setText(path);
	}
	
	public void setDownloadFolder(String path){
		new LabeledText("Download folder:").setText(path);
	}
	
	public void setDeleteArchive(boolean delete){
		if(delete && !new CheckBox("Delete archive after installing").isChecked()){
			new CheckBox("Delete archive after installing").click();
		}
	}

}
