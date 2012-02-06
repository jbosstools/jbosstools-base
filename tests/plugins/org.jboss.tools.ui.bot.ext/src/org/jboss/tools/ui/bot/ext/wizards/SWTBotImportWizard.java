package org.jboss.tools.ui.bot.ext.wizards;

import java.util.Arrays;
import java.util.List;

import org.jboss.tools.ui.bot.ext.SWTBotFactory;
import org.jboss.tools.ui.bot.ext.gen.IImport;


/**
 * Provides actions for opening and navigating import wizard.  
 * 
 * @author Lucia Jelinkova
 *
 */
public class SWTBotImportWizard extends SWTBotWizard {

	public void open(IImport importObject){
		SWTBotFactory.getOpen().newImport(importObject);
	}
	
	public void open(String name, String... path){
		SWTBotFactory.getOpen().newImport(createImportObject(name, path));
	}
	
	private IImport createImportObject(final String name, final String... path){
		return new IImport() {

			@Override
			public String getName() {
				return name;
			}

			@Override
			public List<String> getGroupPath() {
				return Arrays.asList(path);
			}
		};
	}
}
