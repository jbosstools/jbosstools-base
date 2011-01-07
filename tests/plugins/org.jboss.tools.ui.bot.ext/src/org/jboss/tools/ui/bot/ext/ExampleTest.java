package org.jboss.tools.ui.bot.ext;

import static org.eclipse.swtbot.swt.finder.waits.Conditions.shellCloses;

import org.eclipse.swtbot.swt.finder.SWTBot;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotShell;
import org.jboss.tools.ui.bot.ext.gen.ActionItem;
import org.jboss.tools.ui.bot.ext.gen.ActionItem.NewObject.JBossToolsProjectExamples;
import org.jboss.tools.ui.bot.ext.types.IDELabel;
import org.junit.Test;
/**
 * this class represents SWTBot-based Project Example test. Whenever we test project-examples, we should
 * extend these class and override what needed. See esb bot tests for details
 * @author lzoubek
 *
 */
public class ExampleTest extends SWTTestExt{
	/**
	 * returns example project name (as it is imported to workspace)
	 * @return
	 */
	public String getExampleProjectName() {
		return null;
	}
	/**
	 * returns name of example client project (null if none)
	 * @return
	 */
	public String getExampleClientProjectName() {
		return null;
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
	 * <li>{@link ESBExampleTest#importExamples()}</li>
	 * <li>{@link ESBExampleTest#postImport()}</li>
	 * <li>{@link ESBExampleTest#executeExample()}</li>
	 * </ol>
	 */
	@Test
	public void exampleTest() {
		importExamples();
		postImport();
		executeExample();
	}
	/**
	 * runs Project Examples dialog, downloads and imports example's projects to workspace
	 */
	protected void importExamples() {
		SWTBot wiz = open.newObject(JBossToolsProjectExamples.LABEL);
		// wait for progress shell (downloading & parsing example xml)
		SWTBotShell shell = bot.shell("Progress Information");
		shell.activate();
		bot.waitUntil(shellCloses(shell),Timing.time100S());
		open.selectTreeNode(wiz,ActionItem.create(getExampleCategory(),getExampleName()));
		String hasProjName = wiz.textWithLabel(JBossToolsProjectExamples.TEXT_PROJECT_NAME).getText();
		assertTrue(String.format("Example project name changed, have '%s', expected '%s'",hasProjName,getExampleProjectName()),hasProjName.equals(getExampleProjectName()));
		wiz.checkBox("Show the Quick Fix dialog").deselect();
		wiz.button(IDELabel.Button.FINISH).click();
		shell = bot.shell("Downloading...");
		shell.activate();
		bot.waitUntil(shellCloses(shell),Timing.time100S());
		util.waitForNonIgnoredJobs(Timing.time20S());
		assertTrue(String.format("Example project '%s' was not found in project explorer",getExampleProjectName()),projectExplorer.existsResource(getExampleProjectName()));
		if (getExampleClientProjectName()!=null) {
			assertTrue(String.format("Example project '%s' was not found in project explorer",getExampleClientProjectName()),projectExplorer.existsResource(getExampleClientProjectName()));
		}
		
	}
}
