/*******************************************************************************
 * Copyright (c) 2007-2012 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributor:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.ui.bot.ext;

import static org.eclipse.swtbot.swt.finder.finders.UIThreadRunnable.syncExec;
import static org.eclipse.swtbot.swt.finder.matchers.WidgetMatcherFactory.withRegex;
import static org.eclipse.swtbot.swt.finder.matchers.WidgetMatcherFactory.withTooltip;
import static org.junit.Assert.fail;

import java.io.BufferedReader;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintStream;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Properties;

import org.apache.log4j.Logger;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Widget;
import org.eclipse.swtbot.eclipse.finder.SWTWorkbenchBot;
import org.eclipse.swtbot.eclipse.finder.widgets.SWTBotEditor;
import org.eclipse.swtbot.eclipse.finder.widgets.SWTBotView;
import org.eclipse.swtbot.swt.finder.SWTBot;
import org.eclipse.swtbot.swt.finder.exceptions.WidgetNotFoundException;
import org.eclipse.swtbot.swt.finder.finders.MenuFinder;
import org.eclipse.swtbot.swt.finder.finders.UIThreadRunnable;
import org.eclipse.swtbot.swt.finder.results.Result;
import org.eclipse.swtbot.swt.finder.results.WidgetResult;
import org.eclipse.swtbot.swt.finder.utils.SWTBotPreferences;
import org.eclipse.swtbot.swt.finder.utils.SWTUtils;
import org.eclipse.swtbot.swt.finder.waits.ICondition;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotMenu;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotShell;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotToolbarButton;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTree;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTreeItem;
import org.hamcrest.BaseMatcher;
import org.hamcrest.Description;
import org.hamcrest.Matcher;
import org.jboss.tools.ui.bot.ext.parts.SWTBotBrowserExt;
import org.jboss.tools.ui.bot.ext.types.IDELabel;
import org.jboss.tools.ui.bot.ext.types.JobLists;
import org.jboss.tools.ui.bot.ext.types.JobState;
import org.osgi.framework.Bundle;

/**
 * SWTUtilExt is contains helper method for writing SWTBot tests, i.e. various delay methods
 * and wait for jobs etc which don't directly depend on SWTBot.
 * 
 * Note1: These methods should not reference anything from SWTBot directly, if you need to add
 * anything like that that reference it, consider to add into SWTBotExt, SWTEclipseExt, SWTJBTExt
 * or SWTOpenExt
 *
 * Note2: These method are statically accessible in any classes extending SWTTestExt 
 * @author jpeterka
 * 
 */
public class SWTUtilExt extends SWTUtils {

	private static Logger log = Logger.getLogger(SWTUtilExt.class);
	protected SWTWorkbenchBot bot;
	
	private static class AlwaysMatchMatcher<T extends Widget> extends BaseMatcher<T> {

    public boolean matches(Object item) {
      // Always returns true
      return true;
    }

    public void describeTo(Description description) {
      description.appendText("AlwaysMatchMatcher");
    }
	  
	}

	public SWTUtilExt(SWTWorkbenchBot bot) {
		this.bot = bot;
	}

	// ------------------------------------------------------------
	// Waiting methods
	// ------------------------------------------------------------

	// Constants
	final int TIMEOUT = 20000; // 10s
	final int SLEEPTIME = 1000; // 0.5s

	/**
	 * waits until given browser finishes loading web page (handling goRUL or
	 * similar request)
	 * 
	 * @param browser
	 * @return true if browser finished before timeout, false otherwise
	 */
	public boolean waitForBrowserLoadsPage(SWTBotBrowserExt browser) {
		return waitForBrowserLoadsPage(browser, TIMEOUT, null, null);
	}
/**
 * waits until given browser finishes loading page, tests possibility of login dialog
 * @param browser browser which loads page
 * @param login if not null will be possibly used to fill login shell
 * @param pass  if not null will be possibly used to fill login shell
 * @return
 */
	public boolean waitForBrowserLoadsPage(SWTBotBrowserExt browser,
			String login, String pass) {
		return waitForBrowserLoadsPage(browser, TIMEOUT, login, pass);
	}

	/**
	 * waits until given browser finishes with loading page, tests possibility of login dialog
	 * 
	 * @param browser
	 *            browser which loads page
	 * @param timeOut
	 *            total timeout (when reached, false is returned)
	 * @param login
	 *            if not null will be possibly used to fill login shell
	 * @param pass
	 *            if not null will possibly be used to fill login shell
	 */
	public boolean waitForBrowserLoadsPage(SWTBotBrowserExt browser,
			long timeOut, String login, String pass) {

		long startTime = System.currentTimeMillis();
		while (true) {
			if (browser.isPageLoaded()) {
				log.info("Page load finished");
				return true;
			}
			// explore active shell ( authentication window could appear )
			SWTBotShell shell = bot.activeShell();
			if (shell.getText().equalsIgnoreCase("Authentication required")) {
				log.info("Page requies authentication");
				if (login != null && pass != null) {
					try {
						shell.activate();
						SWTBot lBot = shell.bot();
						lBot.text(0).setText(login);
						lBot.text(1).setText(pass);
						lBot.button(IDELabel.Button.OK).click();
						log.info("Authentication window filled");
					} catch (WidgetNotFoundException ex) {
						log.error("Error filling authentication window",ex);
					}
				} else {
					log.error("No credentials provided");
				}
			}

			long waitTime = System.currentTimeMillis() - startTime;
			if ((System.currentTimeMillis() - startTime) > timeOut) {
				log.info("Browser did not load page for " + timeOut / 1000
						+ "s");
				return false;
			}
			log.info("Browser is loading page for " + waitTime / 1000 + "s");
			bot.sleep(SLEEPTIME);
		}

	}
	/**
	 * closes all editor tabs
	 * @param save changes? 
	 */
	public void closeAllEditors(boolean save) {
		for (SWTBotEditor e : bot.editors()) {
			if (save) {
				e.save();
			}
			e.close();
		}
	}
	
	/**
	 * closes all views
	 */
	public void closeAllViews() {
		for (SWTBotView v : bot.views()) {
			v.close();
		}
	}
	
	/**
	 * Wait for named running jobs with defined TIMEOUT
	 */
	public void waitForJobs(String... jobNames) {
		waitForJobs(TIMEOUT, jobNames);
	}

	/**
	 * Wait for all running jobs not named in JobList.ignoredJobs
	 * 
	 * @param timeOut
	 */
	public void waitForNonIgnoredJobs(long timeOut) {
		waitForAllExcept(timeOut, JobLists.ignoredJobs);
	}

	/**
	 * Wait for all running jobs not named in JobList.ignoredJobs
	 */
	public void waitForNonIgnoredJobs() {
		waitForAllExcept(TIMEOUT, JobLists.ignoredJobs);
	}

	/**
	 * Wait for all running jobs
	 */
	public void waitForAll(long timeOut) {
		waitForAllExcept(timeOut, new String[0]);
	}

	/**
	 * Wait for all running jobs
	 * 
	 * @param timeOut
	 */
	public void waitForAll() {
		waitForAllExcept(TIMEOUT, new String[0]);
	}

	/**
	 * Wait for all running jobs except named jobs
	 * 
	 * @param timeOut
	 * @param jobNames
	 */
	public void waitForAllExcept(long timeOut, String... jobNames) {

		// Find all jobs
		Job[] jobs = Job.getJobManager().find(null);
		List<String> listNames = new ArrayList<String>();

		for (Job job : jobs) {
			listNames.add(job.getName());
		}

		// Remove ignored jobs
		for (String jobName : jobNames) {
			if (listNames.contains(jobName))
				listNames.remove(jobName);
		}

		// Create rest job list
		String[] names = new String[listNames.size()];
		for (int i = 0; i < listNames.size(); i++) {
			names[i] = listNames.get(i);
		}

		waitForJobs(timeOut, names);
	}

	/**
	 * Waits for selected job
	 * 
	 * @param timeOut
	 * @param jobNames
	 */
	public void waitForJobs(long timeOut, String... jobNames) {

		// DEBUG
		printRunningJobs();

		// No Jobs
		if (jobNames == null || jobNames.length == 0) {
			log.info("No jobs prescribed as blocking");
			delay();
			return;
		}

		// Jobs prescribed
		long startTime = System.currentTimeMillis();
		List<String> blockingJobs = new ArrayList<String>();

		// Go through jobs and wait for completion
		for (String jobName : jobNames) {
			if (isJobRunning(jobName)) {
				log.info("Blocking job " + jobName + " found");
				blockingJobs.add(jobName);
			}
		}

		// Wait until all blocking jobs aren't finished or timeout
		for (String jobName : blockingJobs) {
			while (true) {
				if (!isJobRunning(jobName)) {
					log.info("Job  " + jobName + " is finished");
					break;
				}

				long waitTime = System.currentTimeMillis() - startTime;
				if ((System.currentTimeMillis() - startTime) > timeOut) {
					log.info("Waiting for job " + jobName + " timeOut "
							+ timeOut + "ms");
					break;
				}
				log.info("Job \"" + jobName + "\" is running for " + waitTime
						/ 1000 + "s");
				bot.sleep(SLEEPTIME);
			}
		}

		log.info("All blocking jobs finished or skipped");
	}

	/**
	 * Search for jobname in JobManager job list
	 * 
	 * @param jobName
	 *            name of the job
	 * @return true if job with corresponding name found, else false
	 */
	private boolean isJobRunning(String jobName) {
		Job[] jobs = Job.getJobManager().find(null);
		for (Job job : jobs) {
			if ((jobName.equalsIgnoreCase(job.getName()))
					&& ((job.getState() == JobState.RUNNING) || (job.getState() == JobState.WAITING)))
				return true;
		}
		return false;
	}

	public void printRunningJobs() {
		Job[] jobs = Job.getJobManager().find(null);
		for (Job job : jobs) {

			String jobStateName = JobState.getStateName(job.getState());

			log.info("Active Job: P:" + job.getPriority() + "\" S:"
					+ jobStateName + "\" R:" + job.getResult() + "\" N:\""
					+ job.getName() + "\"");
		}
	}

	public void delay() {
		bot.sleep(500);
	}

	// ------------------------------------------------------------
	// Resource methods
	// ------------------------------------------------------------

	/**
	 * Get resource file
	 */
	public static File getResourceFile(String pluginId, String... path) {

		// Construct path
		StringBuilder builder = new StringBuilder();
		for (String fragment : path) {
			builder.append("/" + fragment);
		}

		String filePath = "";
		try {
			filePath = FileLocator.toFileURL(
					Platform.getBundle(pluginId).getEntry("/")).getFile()
					+ "resources" + builder.toString();
			File file = new File(filePath);
			if (!file.isFile()) {
				filePath = FileLocator.toFileURL(
						Platform.getBundle(pluginId).getEntry("/")).getFile()
						+ builder.toString();
			}
		} catch (IOException ex) {
			String message = filePath + " resource file not found";
			//log.error(message);
			fail(message);
		}

		File file = new File(filePath);
		return file;
	}

	/**
	 * Reads text file
	 * 
	 * @param file
	 * @return
	 */
	public String readTextFile(File file) {

		StringBuilder contents = new StringBuilder();

		try {
			BufferedReader input = new BufferedReader(new FileReader(file));
			try {
				String line = null; // not declared within while loop
				while ((line = input.readLine()) != null) {
					contents.append(line);
					contents.append(System.getProperty("line.separator"));
				}
			} finally {
				input.close();
			}
		} catch (IOException ex) {
			ex.printStackTrace();
		}

		return contents.toString();
	}

	/**
	 * Loads project property file for particular test plugin
	 * 
	 * @param pluginId
	 * @return
	 */
	public Properties loadProperties(String pluginId) {
		Properties properties = new Properties();
		try {
			log.info("Loading properties for " + pluginId);
			// Read project properties
			Bundle bundle = Platform.getBundle(pluginId);
			InputStream is = bundle.getResource("project.properties")
					.openStream();
			properties.load(is);
			log.info("Properties for " + pluginId + " loaded:");

		} catch (Exception ex) {
			logAndFail("Problem with loading properties file");
		}
		return properties;
	}

	/**
	 * Get value from property file with error logging
	 * 
	 * @param properties
	 * @param key
	 * @return
	 */
	public String getValue(Properties properties, String key) {
		String value = properties.getProperty(key);
		if ((value == null) || value.equalsIgnoreCase("")) {
			logAndFail("Missing property value for key \"" + key + "\"");
		}
		return value;
	}

	/**
	 * Check Property values
	 * 
	 * @param properties
	 * @param key
	 */
	public void checkAndLogValue(Properties properties, String key) {
		log.info(key + "=" + getValue(properties, key));
	}

	/**
	 * Log and fail
	 * 
	 * @param msg
	 */
	public void logAndFail(String msg) {
		log.error(msg);
		fail(msg);
	}

	/**
	 * Write all running processes names to log
	 */
	public void logAllRunningProcesses() {
		Job[] jobs = Job.getJobManager().find(null);

		for (Job job : jobs) {
			log.info(job.getName());
		}
	}

	/**
	 * Write all menu items of menu to log
	 * 
	 * @param menu
	 */
	public void logAllSubmenus(MenuItem menuItem) {

		final MenuItem miTmp = menuItem;

		menuItem.getDisplay().asyncExec(new Runnable() {
			public void run() {
				int index = 0;
				for (MenuItem miSubmenu : miTmp.getMenu().getItems()) {
					log.info(index++ + ": " + miSubmenu);
				}

			}
		});

	}

	/**
	 * Returns Test Plugin Location within file system
	 * 
	 * @param projectName
	 * @return
	 */
	public static String getTestPluginLocation(String projectName) {

		String[] parts = System.getProperty("eclipse.commands").split("\n");

		int index = 0;

		for (index = 0; parts.length > index + 1
				&& !parts[index].equals("-data"); index++) {
			// do nothing just go through
		}

		return parts[index + 1] + File.separator + projectName;

	}

	/**
	 * Returns true if shell with shellTitle is active
	 * 
	 * @param shellTitle
	 * @param bot
	 * @return
	 */
	public static boolean isShellActive(String shellTitle, SWTWorkbenchBot bot) {
		boolean isShellActive = false;
		try {
			bot.shell(shellTitle).activate();
			isShellActive = true;
		} catch (WidgetNotFoundException e) {
		}
		return isShellActive;
	}

	/**
	 * Returns true if shell with shellTitle is active
	 * 
	 * @param shellTitle
	 * @return
	 */
	public boolean isShellActive(String shellTitle) {
		return SWTUtilExt.isShellActive(shellTitle, bot);
	}

	/**
	 * Closes shell with shellTitle if shell is active
	 * 
	 * @param shellTitle
	 * @param bot
	 * @return
	 */
	public static void closeShellWhenActive(String shellTitle,
			SWTWorkbenchBot bot) {
		try {
			bot.shell(shellTitle).close();
		} catch (WidgetNotFoundException e) {
		}
	}

	/**
	 * Closes shell with shellTitle if shell is active
	 * 
	 * @param shellTitle
	 * @return
	 */
	public void closeShellWhenActive(String shellTitle) {
		SWTUtilExt.closeShellWhenActive(shellTitle, bot);
	}
  /**
   * Returns Properties which contains Virtual Machine arguments
   * with name starting with "-D"
   * @return
   */
  public static Properties parseEclipseVMArgs (){
    
    Properties vmArgsProps = new Properties();
    
    String vmArgs = System.getProperty("eclipse.vmargs");
    
    if (vmArgs != null){
      for (String line : vmArgs.split("\n")){
        if (line.startsWith("-D")){
          String[] splitLine = line.split("=");
          vmArgsProps.setProperty(splitLine[0], splitLine[1]);
        }
      }
    }
    
    return vmArgsProps;
    
  }
  /**
   * Overrides propertyName property value within properties with value stored within vmargProperties with name vmargPropertyName
   * @param properties
   * @param propertyName
   * @param vmargPropertyName
   * @param vmargProperties
   */
  public static void overrideValueFromSystemProperty (Properties properties, String propertyName , 
    String vmargPropertyName, Properties vmargProperties){
    
    String vmargProperty = vmargProperties.getProperty(vmargPropertyName);
    if (vmargProperty != null){
      properties.setProperty(propertyName, vmargProperty);
    }
    
  }
  /**
   * Delete directory from directoryLocation
   * @param directoryLocation
   */
  public static void deleteDirectory(String directoryLocation){
    File directory = new File (directoryLocation);
    if (directory.exists()){
      if (directory.isDirectory()){
        for (File childFile :directory.listFiles()) {
          SWTUtilExt.deleteDirectory(childFile.getAbsolutePath());
        }
        directory.delete();
      }
      else{
        directory.delete();
      }
    }
  }
  /**
   * gets all active widgets as string 
   * @param bot
   * @return
   */
  public static String getAllBotWidgetsAsText(final SWTBot bot) {
	  return UIThreadRunnable.syncExec(new Result<String>(){
		public String run() {
			StringBuilder sb = new StringBuilder();
			List<?> widgets = bot.widgets(new SWTUtilExt.AlwaysMatchMatcher<Widget>());
		    for (Object object : widgets){
		    	String objectName = object.getClass().getSimpleName();
		    	String text = SWTUtilExt.invokeMethod(object, "getText").trim();
		    	if (!"".equals(text)) {
		    		sb.append("{"+ objectName + "->"+text+"} ");
		    	}
		    }
		  return sb.toString();
		}});
	  
  }
  /**
   * Display all active widgets
   * @param bot
   */
  public static void displayAllBotWidgets (SWTBot bot){
    List<?> widgets = bot.widgets(new SWTUtilExt.AlwaysMatchMatcher<Widget>());
    for (Object object : widgets){
      String objectToString;
      try{
        objectToString = object.toString(); 
      } catch (Throwable t){
        objectToString = "<null>";
      }
      System.out.println(objectToString  +  
        " Text: " + SWTUtilExt.invokeMethod(object, "getText") +
        " Tooltip: " + SWTUtilExt.invokeMethod(object, "getToolTipText"));
    }
  }
  /**
   * Display all active widgets within parent widget
   * @param bot
   * @param parent
   */
  public static void displayAllBotWidgets (SWTBot bot, Widget parent){
    List<?> widgets = bot.widgets(new SWTUtilExt.AlwaysMatchMatcher<Widget>(),parent);
    for (Object object : widgets){
      System.out.println(object + 
        " Class: " + object.getClass() +  
        " Text: " + SWTUtilExt.invokeMethod(object, "getText") +
        " Tooltip: " + SWTUtilExt.invokeMethod(object, "getToolTipText"));
    }
  }
  /**
   * Display all Toolbar Buttons of view
   * @param view 
   */
  public static void displayAllToolbarButtons (SWTBotView view){
    List<SWTBotToolbarButton> buttons = view.getToolbarButtons();
    for (SWTBotToolbarButton button : buttons){
      System.out.println("Button Tooltip: " + button.getToolTipText() +
        " Text: " + button.getText() + 
        " Class: " + button.getClass());
    }
  }
  /**
   * Displays all tree items of parentTreeItem node
   * @param tree
   * @param parentTreeItem
   */
  public static void displayAllTreeItemChildren (SWTBotTree tree,SWTBotTreeItem parentTreeItem){
    SWTBotTreeItem[] items = parentTreeItem.getItems();
    if (items != null){
      for (int index = 0 ; index < items.length ; index++){
        StringBuffer columnsText = new StringBuffer("");
        for (int column = 0;column < tree.columnCount(); column++){
          if (column > 0){
            columnsText.append(" ");
          }
          columnsText.append(column);
          columnsText.append("[");
          columnsText.append(items[index].cell(column));
          columnsText.append("]");
        }
        System.out.println("TreeItem text: " + items[index].getText() + 
          " tooltip: " + items[index].getToolTipText() +
          " columns: " + columnsText.toString());
      }
    }
  }
  /**
   * Returns Toolbar Button of view with specified toolTip
   * @param view 
   * @param toolTip
   */
  public static SWTBotToolbarButton getViewToolbarButtonWithTooltip (SWTBotView view, String toolTip){
    List<SWTBotToolbarButton> buttons = view.getToolbarButtons();
    SWTBotToolbarButton result = null;
    if (buttons != null){
      Matcher<Widget> withTooltip = withTooltip(toolTip);
      Iterator<SWTBotToolbarButton> iterator = buttons.iterator();
      while (iterator.hasNext() && result == null){
        SWTBotToolbarButton button = iterator.next();
        if (withTooltip.matches(button)){
          result = button;  
        }
      }
    }  
    
    if (result == null){
      throw new WidgetNotFoundException("Unable to find Toolbar Button with ToolTip " +
        toolTip);      
    }else{
      return result;
    }
    
  }
  /**
   * Invoke method on object and returns result as String
   * @param object
   * @param method
   * @return
   */
  public static String invokeMethod (Object object, String method){
    
    String result = "<null>";
    
    try {
      Object oResult = SWTUtils.invokeMethod(object, method);
      if (oResult != null){
        result = oResult.toString();
      }
      else{
        result = "<null>";
      }
    } catch (NoSuchMethodException e) {
      result = "<null>";
    } catch (IllegalAccessException e) {
      result = "<null>";
    } catch (InvocationTargetException e) {
      result = "<null>";
    }
    
    return result;
  }
  /**
   * Invoke method on object and returns result as Object
   * @param object
   * @param method
   * @return
   */
  public static Object invokeMethodReturnObject (Object object, String method){
    
    String result = "<null>";
    
    try {
      return SWTUtils.invokeMethod(object, method);
    } catch (NoSuchMethodException e) {
      result = "<null>";
    } catch (IllegalAccessException e) {
      result = "<null>";
    } catch (InvocationTargetException e) {
      result = "<null>";
    }
    
    return result;
  }
  /**
   * Returns location of file within plugin 
   * @param pluginId
   * @param fileName
   * @return
   */
  public static String getPathToFileWithinPlugin(String pluginId,
      String fileName) {

    String filePath = null;
    try {
      filePath = FileLocator.toFileURL(
          Platform.getBundle(pluginId).getEntry("/")).getFile()
          + fileName;
    } catch (IOException e) {
      // do nothing filePath is null
      e.printStackTrace();
    }

    return filePath;
  }
  /**
   * Returns path to project with projectName
   * @param projectName
   * @return
   */
  public static String getPathToProject(String projectName) {
    return ResourcesPlugin.getWorkspace().getRoot().getLocation().toString() +
      File.separator +
      projectName;
  }

    /**
     * Returns the first found menu which matches given regular expression.
     * 
     * @param menuRegex Regular expression which is looking for in menu text.
     * @param parentMenu Parent menu which is used for searching.
     * 
     * @return Menu matching regular expression.
     * 
     * @throws WidgetNotFoundException If no menu matches.
     */
    public static SWTBotMenu menuWithRegex(final String menuRegex, final SWTBotMenu parentMenu) throws WidgetNotFoundException {
        final Matcher<? extends Widget> matcher = withRegex(menuRegex);
        MenuItem menuItem = syncExec(new WidgetResult<MenuItem>() {
            public MenuItem run() {
                Menu bar = parentMenu.widget.getMenu();
                Matcher<MenuItem> withRegex = withRegex(menuRegex);
                List<MenuItem> menus = new MenuFinder().findMenus(bar, withRegex, true);
                if (!menus.isEmpty())
                    return menus.get(0);
                return null;
            }
        });
        return new SWTBotMenu(menuItem, matcher);
    }

    /********** CAPTURING OF STANDARD OUTPUT **********/

    /**
     * Default output stream before redirecting.
     */
    private static PrintStream defaultOutputStream = System.out;
    /**
     * Redirected output stream for capturing output.
     */
    private static ByteArrayOutputStream capturingByteArrayOutputStream = null;

    /**
     * Starts capturing of standard output.
     * Redirects standard output into other <code>PrintStream</code>
     * which is captured.
     */
    public static void startCapturingStandardOutput() {
        capturingByteArrayOutputStream = new ByteArrayOutputStream();
        System.setOut(new PrintStream(capturingByteArrayOutputStream));
        log.info("Capturing of standard output was started.");
    }

    /**
     * Stops capturing of standard output by setting the default standard output.
     * 
     * @return Captured output.
     */
    public static String stopCapturingStandardOutput() {
        System.setOut(defaultOutputStream);
        final String capturedOutput = new String(capturingByteArrayOutputStream.toByteArray());
        capturingByteArrayOutputStream = null;
        log.info("Capturing of standard output was stopped.");
        return capturedOutput;
    }

    /********** END OF CAPTURING OF STANDARD OUTPUT **********/
    
    /**
     * Waits until toolbarButton with timeout SWTBotPreferences.TIMEOUT  
     * @param toolbarButton
     */
    public void waitForToolbarButtonEnabled (SWTBotToolbarButton toolbarButton) {
      waitForToolbarButtonEnabled(toolbarButton, SWTBotPreferences.TIMEOUT);
    }
    /**
     * Waits until toolbarButton is enabled with timeout  
     * @param toolbarButton
     */
    public void waitForToolbarButtonEnabled (final SWTBotToolbarButton toolbarButton, final long timeout) {
      bot.waitUntil(new ICondition() {
        
        @Override
        public boolean test() throws Exception {
          return toolbarButton.isEnabled();
        }
        
        @Override
        public void init(SWTBot bot) {
        }
        
        @Override
        public String getFailureMessage() {
          return "Tooolbar button with tooltip " + toolbarButton.getToolTipText() +
              " was not enabled within " + timeout + " miliseconds";
        }
      },
      timeout);
    }
    /**
     * Waits while toolbarButton is disabled with timeout SWTBotPreferences.TIMEOUT  
     * @param toolbarButton
     */
    public void waitWhileToolbarButtonisDisabled (SWTBotToolbarButton toolbarButton) {
      waitWhileToolbarButtonisDisabled(toolbarButton, SWTBotPreferences.TIMEOUT);
    }
    /**
     * Waits while toolbarButton is disabled with timeout  
     * @param toolbarButton
     */
    public void waitWhileToolbarButtonisDisabled (final SWTBotToolbarButton toolbarButton, final long timeout) {
      bot.waitWhile(new ICondition() {
        
        @Override
        public boolean test() throws Exception {
          return toolbarButton.isEnabled();
        }
        
        @Override
        public void init(SWTBot bot) {
        }
        
        @Override
        public String getFailureMessage() {
          return "Tooolbar button with tooltip " + toolbarButton.getToolTipText() +
              " was not enabled within " + timeout + " miliseconds";
        }
      },
      timeout);
    }
    /**
     * Waits until toolbarButton is with tooltip is found with timeout  
     * @param tooltip
     * @param timeout
     */
    public void waitForToolbarButtonWithTooltipIsFound (final String tooltip, final long timeout) {
      bot.waitUntil(new ICondition() {
        
        @Override
        public boolean test() throws Exception {
          boolean toolbarButtonIsFound = false;
          try {
            bot.toolbarButtonWithTooltip(tooltip);
            toolbarButtonIsFound = true;
          }
          catch (WidgetNotFoundException wnfe){
            toolbarButtonIsFound = false;
          }
          return toolbarButtonIsFound;
        }
        
        @Override
        public void init(SWTBot bot) {
        }
        
        @Override
        public String getFailureMessage() {
          return "Tooolbar button with tooltip " + tooltip +
              " was not available " + timeout + " miliseconds";
        }
      },
      timeout);
    }
}