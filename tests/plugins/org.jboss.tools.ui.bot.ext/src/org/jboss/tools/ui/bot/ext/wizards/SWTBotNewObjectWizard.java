package org.jboss.tools.ui.bot.ext.wizards;

import java.util.Arrays;
import java.util.List;

import org.jboss.tools.ui.bot.ext.SWTBotFactory;
import org.jboss.tools.ui.bot.ext.gen.INewObject;

/**
 * Provides actions for opening and navigating new object wizard.  
 * 
 * @author Lucia Jelinkova
 *
 */
public class SWTBotNewObjectWizard extends SWTBotWizard {

	public void open(INewObject newObject){
		SWTBotFactory.getOpen().newObject(newObject);
	}
	
	public void open(String name, String... path){
		SWTBotFactory.getOpen().newObject(createNewObject(name, path));
	}
	
	private INewObject createNewObject(final String name, final String... path){
		return new INewObject() {

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
