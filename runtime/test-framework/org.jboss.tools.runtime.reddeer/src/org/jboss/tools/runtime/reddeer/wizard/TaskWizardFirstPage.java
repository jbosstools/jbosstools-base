package org.jboss.tools.runtime.reddeer.wizard;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.reddeer.common.logging.Logger;
import org.eclipse.reddeer.jface.wizard.WizardPage;
import org.eclipse.reddeer.swt.api.Table;
import org.eclipse.reddeer.swt.impl.table.DefaultTable;

public class TaskWizardFirstPage extends WizardPage{
    
    	private static Logger log = new Logger(TaskWizardFirstPage.class);
	
	public Map<String,String> getDownloadableRuntimes(){
		Table runtimesTable = new DefaultTable();
		Map<String,String> runtimes = new HashMap<String, String>();
		for(int i=0; i<runtimesTable.rowCount();i++){
			runtimes.put(runtimesTable.getItem(i).getText(),runtimesTable.getItem(i).getText(1));	
		}
		return runtimes;
	}
	
	public void selectRuntime(String runtimeName){
	    	DefaultTable defaultTable = new DefaultTable();
	    	defaultTable.getItems().forEach(ti -> log.trace(ti.getText())); //debug output
	    	defaultTable.select(runtimeName);

	}

}
