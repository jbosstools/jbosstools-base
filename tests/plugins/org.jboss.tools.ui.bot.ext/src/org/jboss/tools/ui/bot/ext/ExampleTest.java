package org.jboss.tools.ui.bot.ext;

import static org.eclipse.swtbot.swt.finder.waits.Conditions.shellCloses;
import static org.eclipse.swtbot.swt.finder.waits.Conditions.shellIsActive;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.eclipse.swtbot.swt.finder.SWTBot;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotShell;
import org.jboss.tools.ui.bot.ext.gen.ActionItem;
import org.jboss.tools.ui.bot.ext.gen.ActionItem.NewObject.JBossToolsProjectExamples;
import org.jboss.tools.ui.bot.ext.types.IDELabel;
import org.junit.Test;

import org.jboss.tools.ui.bot.ext.config.ConfiguredState;

/**
 * this class represents SWTBot-based Project Example test. Whenever we test project-examples, we should
 * extend these class and override what needed. See esb bot tests for details
 * @author lzoubek
 *
 */
public class ExampleTest extends SWTTestExt{

	/**
	 * returns list of example projects
	 * @return
	 */
	public String[] getProjectNames() {
		return new String[]{"example"};
	}
	/**
	 * gets example name (listed in project examples dialog)
	 * @return
	 */
	public String getExampleName() {
		return null;
	}
	/**
	 * gets example category (listed in project examples dialog)
	 * @return
	 */
	public String getExampleCategory() {
		return null;
	}
	/**
	 * override to implement post-import step (fix dependencies etc.)
	 */
	protected void postImport() {}
	/**
	 * override to implement example execution (deployment, running asserts)
	 */
	protected void executeExample() {}
	/**
	 * main test method, all steps can be overriden
	 * <ol>
	 * <li>{@link ESBExampleTest#importExample()}</li>
	 * <li>{@link ESBExampleTest#postImport()}</li>
	 * <li>{@link ESBExampleTest#executeExample()}</li>
	 * </ol>
	 */
	@Test
	public void exampleTest() {
		SWTBot wiz;
		// consider test as passed if example is not found
		if ((wiz = existsExample()) != null) {
			importExample(wiz);
			postImport();
			executeExample();	
		} else if (failOnMissingExample()){
			throw new AssertionError("The example " + getExampleName() + " was not found");
		}
	}
	
	/**
	 * The default behavior is to finish the thest gracefully if the example does not exist. 
	 * When this behavior is not desirable, subclasses can change the behavior by overriding this
	 * method. 
	 */
	protected boolean failOnMissingExample(){
		return false;
	}
	
	private SWTBot existsExample() {

		/* ldimaggi - Sept 27, 2012 - JBQA-7081 - Need to add this to deal with the
		 * extra UI window (un-named) that is opened for the SOA/ESB tests during the
		 * startup of an SOA-P server in eclipse/JBDS.
		 */
//		SWTBotShell [] theShells = bot.shells();
//		for (SWTBotShell s : theShells) {
//			System.out.println ("DEBUG! " + s.getText());
//		}
		
		log.info("Example name=" + getExampleName());
		log.info("SWTJBTExt.isRunningOnMacOs() = " + SWTJBTExt.isRunningOnMacOs());
		log.info("bot.shell(\"Java - Eclipse Platform\").isActive()" + bot.shell("Java - Eclipse Platform").isActive() );
		log.info("configuredState.getServer().type" + configuredState.getServer().type);
		
		if ( (SWTJBTExt.isRunningOnMacOs()) && (!bot.shell("Java - Eclipse Platform").isActive()) && (configuredState.getServer().type.equals("SOA"))  ){
			log.info("Running on a MAC - need to set Focus back to the Eclipse shell window");
			bot.sleep(30000l);
			bot.shell("Java - Eclipse Platform").activate();
			bot.shell("Java - Eclipse Platform").setFocus();
		}
		
		SWTBot wiz = open.newObject(JBossToolsProjectExamples.LABEL);
		log.info("new object opened");
		// wait for progress shell (downloading & parsing example xml)
		bot.waitWhile(shellIsActive("Progress Information"), Timing.time100S());
		try {
			open.selectTreeNode(wiz,ActionItem.create(getExampleCategory(),getExampleName()));
			return wiz;
		}
		catch (Exception ex) {
			log.warn(String.format("Project example %s under category %s was not found",getExampleName(),getExampleCategory()));
			return null;
		}
	}
	/**
	 * runs Project Examples dialog, downloads and imports example's projects to workspace
	 */
	protected void importExample(SWTBot wiz) {
		String hasProjName = wiz.textWithLabel(JBossToolsProjectExamples.TEXT_PROJECT_NAME).getText();
		
		//assertTrue(String.format("Example project name changed, have '%s', expected '%s'",hasProjName,getProjectNames()[0]),hasProjName.equals(getProjectNames()[0]));
		int projSize = getProjectSize(wiz.textWithLabel(JBossToolsProjectExamples.TEXT_PROJECT_SIZE).getText());
		wiz.button(IDELabel.Button.FINISH).click();
				
		SWTBotShell shell = bot.shell("Downloading...");
		shell.activate();
		log.info("wait until shell closes");
		bot.waitUntil(shellCloses(shell),Timing.time(projSize*20*1000));
		log.info("wait for ignored jobs");
		util.waitForNonIgnoredJobs(Timing.time20S());
				
		/* ldimaggi - Oct 8, 2012 */
		try {
			bot.waitForShell("New Project Example");		
			bot.shell("New Project Example").bot().button(IDELabel.Button.FINISH).click();	
		}
		catch (Exception E) {
			log.error("Cannot find New Project Example dialog - debugging Mac failures on Jenkins" + E.getMessage());
		}
		
		bot.sleep(Timing.time5S());			
		/* Temporary fix needed by - https://issues.jboss.org/browse/JBIDE-11781 
		 * Close the Quick Fix shell/dialog - or the project explorer cannot find the
		 * webservice_producer|client projects in the project explorer - ldimaggi - May 2012 */
		try {
			bot.shell("Quick Fix").bot().button(IDELabel.Button.FINISH).click();
		}
		catch (Exception E) {
			log.info("Condition from https://issues.jboss.org/browse/JBIDE-11781 not found " + E.getMessage());
		}
		bot.sleep(Timing.time5S());			
		
		for (String project : getProjectNames()) {
			assertTrue(String.format("Example project '%s' was not found in project explorer",project),projectExplorer.existsResource(project));
		}		
	}
	private int getProjectSize(String size) {
		Pattern pattern = Pattern.compile("([\\d\\.]+)(M|K)");
		Matcher m = pattern.matcher(size);
		if (m.matches()) {
			try {
				double s = Double.parseDouble(m.group(1));
				if ("M".equals(m.group(2))) {
					return (int)s*1024;
				}
				return (int)s;
			}
			catch (Exception e) {
				return 1000;
			}
		}
		
		return 1000;
	}
}
