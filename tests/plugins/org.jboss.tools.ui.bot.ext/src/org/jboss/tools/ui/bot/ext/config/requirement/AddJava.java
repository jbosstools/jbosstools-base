package org.jboss.tools.ui.bot.ext.config.requirement;

import org.jboss.tools.ui.bot.ext.SWTTestExt;
import org.jboss.tools.ui.bot.ext.config.TestConfigurator;
/**
 * adds JRE among Installed JRE's
 * @author lzoubek
 *
 */
public class AddJava extends RequirementBase {

	/**
	 * requirement for adding JRE/JDK
	 * @param user to add
	 */
	public AddJava() {
	}
	@Override
	public boolean checkFulfilled() {
		return SWTTestExt.configuredState.getJreList().contains(getAddedAsName());
	}

	@Override
	public void handle(){
		SWTTestExt.eclipse.addJavaVM(getAddedAsName(), getJavaHome());
		SWTTestExt.configuredState.getJreList().add(getAddedAsName());
	}
	public String getAddedAsName() {
		return "JRE-"+TestConfigurator.currentConfig.getJava().version;
	}
	private String getJavaHome() {
		return TestConfigurator.currentConfig.getJava().javaHome;
	}

}
