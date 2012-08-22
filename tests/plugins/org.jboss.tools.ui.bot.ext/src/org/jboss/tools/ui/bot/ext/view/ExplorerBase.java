package org.jboss.tools.ui.bot.ext.view;

import java.util.List;
import java.util.Vector;

import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swtbot.eclipse.finder.widgets.SWTBotEditor;
import org.eclipse.swtbot.swt.finder.SWTBot;
import org.eclipse.swtbot.swt.finder.exceptions.WidgetNotFoundException;
import org.eclipse.swtbot.swt.finder.finders.UIThreadRunnable;
import org.eclipse.swtbot.swt.finder.results.WidgetResult;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotMenu;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotShell;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTree;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTreeItem;
import org.jboss.tools.ui.bot.ext.SWTEclipseExt;
import org.jboss.tools.ui.bot.ext.Timing;
import org.jboss.tools.ui.bot.ext.helper.ContextMenuHelper;
import org.jboss.tools.ui.bot.ext.types.IDELabel;
/**
 * base class for explorer-like helper components
 * @author lzoubek
 *
 */
public abstract class ExplorerBase extends ViewBase {

	public ExplorerBase() {

	}
	/*
	 * Selects given project in Package Explorer
	 */
	public void selectProject(String projectName) {
		bot().tree().expandNode(projectName).select();
  }

  /**
   * Selects Tree Item within Package Explorer
   * 
   * @param timeOut
   * @param treeItemText
   * @param path
   * @return
   */
  public SWTBotTreeItem selectTreeItem(int timeOut, String treeItemText,
      String[] path) {
    SWTBot viewBot = bot();
    return SWTEclipseExt.getTreeItemOnPath(viewBot, viewBot.tree(), timeOut,
        treeItemText, path).select();
  }
  /**
   * Selects Tree Item within Package Explorer
   * 
   * @param treeItemText
   * @param path
   * @return
   */
  public SWTBotTreeItem selectTreeItem(String treeItemText,
      String[] path) {
    return selectTreeItem(0,treeItemText,path);
  }
	/**
	 * deletes given project from workspace
	 * @param projectName
	 * @param fileSystem if true, project will be also deleted from file-system
	 */
	public void deleteProject(String projectName, boolean fileSystem) {
		log.info("Deleting project '"+projectName+"'");
		SWTBot viewBot = show().bot();
		SWTBotTreeItem item = viewBot.tree().expandNode(projectName);
		ContextMenuHelper.prepareTreeItemForContextMenu(viewBot.tree(), item);
		new SWTBotMenu(ContextMenuHelper.getContextMenu(viewBot.tree(), IDELabel.Menu.DELETE, false)).click();
	     SWTBotShell shell = bot.shell("Delete Resources").activate();
	     if (fileSystem) {
	    	 shell.bot().checkBox().click();
	     }
	     open.finish(shell.bot(),IDELabel.Button.OK);
	     util.waitForNonIgnoredJobs();
	     //util.waitForAll(Timing.time30S()); rhopp. Removed unnecessary waiting.
	}
	/**
	 * deletes all projects from workspace
	 */
	public void deleteAllProjects() {
		SWTBot viewBot = show().bot();
		    List<String> items = new Vector<String>();
		    for (SWTBotTreeItem ti : viewBot.tree().getAllItems()) {
		    	items.add(ti.getText());
		    }
		    for (String proj : items) {
		    	try {
		    		viewBot.tree().expandNode(proj);
		    		viewBot.tree().select(proj);
		    		// try to select project in tree (in some cases, when one project is deleted, 
		    		// the other item in tree (not being a project) is auto-deleted)
		    		
		    	} catch (WidgetNotFoundException ex) {
		    		log.warn("Attempted to delete non-existing project '"+proj+"'");
		    		continue;
		    	}
		    	deleteProject(proj,true);
		    }
	}
	/**
	 * opens file (selects in tree and doubleclicks)
	 * @param timeOut
	 * @param projectName
	 * @param path to file
	 * @return editor with opened file
	 */
	public SWTBotEditor openFile(int timeOut, String projectName, String... path) {
		SWTBot viewBot = show().bot();
		SWTBotTree tree = viewBot.tree();
		SWTBotTreeItem item = tree.expandNode(projectName);
		viewBot.sleep(timeOut);
		StringBuilder builder = new StringBuilder(projectName);
		// Go through path
		for (String nodeName : path) {
			item = item.expandNode(nodeName);
			viewBot.sleep(timeOut);
			builder.append("/" + nodeName);
		}
		item.select().doubleClick();
		log.info("File Opened:" + builder.toString());
    SWTBotEditor editor = null;
    // Try to find editor containing opened file
    try{
      editor = bot.editorByTitle(path[path.length - 1]);  
    } catch (WidgetNotFoundException wnfe){
      // in case name of opened file is different then editor title of editor containing opened file
      editor = bot.activeEditor();
    }
		return editor;
	}
	/**
   * opens file (selects in tree and doubleclicks) with zero timeOut for expandNode() method
   * @param projectName
   * @param path to file
   * @return editor with opened file
   */
  public SWTBotEditor openFile(String projectName, String... path) {
    return openFile(0,projectName,path);
  }
	/**
	 * runs given project on Server (uses default server, the first one) server MUST be running
	 * @param projectName
	 */
	public void runOnServer(String projectName) {
		runOnServer(projectName, null);
	}
	
	/**
	 * runs given file on Server (uses default server, the first one) server MUST be running
	 * @param projectName
	 */
	public void runOnServer(String projectName, String fileName, String... path) {
		SWTBot viewBot = show().bot();
		SWTBotTreeItem item;
		
		if (fileName == null){
			item = viewBot.tree().expandNode(projectName);
		} else {
			item = SWTEclipseExt.getTreeItemOnPath(viewBot, viewBot.tree(), 0, fileName, path);	
		}
		
		item.select();
		ContextMenuHelper.prepareTreeItemForContextMenu(viewBot.tree(), item);
		   final SWTBotMenu menuRunAs = viewBot.menu(IDELabel.Menu.RUN).menu(IDELabel.Menu.RUN_AS);
		    final MenuItem menuItem = UIThreadRunnable
		      .syncExec(new WidgetResult<MenuItem>() {
		        public MenuItem run() {
		          int menuItemIndex = 0;
		          MenuItem menuItem = null;
		          final MenuItem[] menuItems = menuRunAs.widget.getMenu().getItems();
		          while (menuItem == null && menuItemIndex < menuItems.length){
		        	  log.info("Found item" +menuItems[menuItemIndex].getText());
		            if (menuItems[menuItemIndex].getText().indexOf("Run on Server") > - 1){
		              menuItem = menuItems[menuItemIndex];
		            }
		            else{
		              menuItemIndex++;
		            }
		          }
		        return menuItem;
		        }
		      });
		    if (menuItem != null){
		      new SWTBotMenu(menuItem).click();
		      SWTBotShell shell = bot.shell(IDELabel.Shell.RUN_ON_SERVER).activate();
		      open.finish(shell.bot());		      
		      util.waitForAll(Timing.time3S());
		    }
		    else{
		      throw new WidgetNotFoundException("Unable to find Menu Item with Label 'Run on Server'");
		    }
		
	}
	/**
	 * true if resource described by parameters exists in ProjectExplorer
	 * @param projectName project name
	 * @param resource path (e.g. 'Project' 'src' 'org.jbosstools.test' 'MyClass.java')
	 * @return 
	 */
	public boolean existsResource(String... resource) {
		
		try {
			SWTBot viewBot = show().bot();
			SWTBotTreeItem ancestor = viewBot.tree().getTreeItem(resource[0]);
			viewBot.tree().expandNode(resource[0]);
			for (int i=1;i<resource.length;i++) {
				ancestor = getItem(ancestor, resource[i]);
				if (ancestor == null) {
					return false;
				}				
			}
			return true;
			}
			catch (WidgetNotFoundException ex) {
				return false;
			}
	}
	
	
	/***
	 * Checks presence of file
	 * 
	 * @param projectName - project name
	 * @param path - path to file
	 * @return true if file is located in explorer, false if not
	 */
	public boolean isFilePresent(String projectName, String... path) {
		SWTBot viewBot = open.viewOpen(viewObject).bot();
		SWTBotTree tree = viewBot.tree().select(projectName);
		StringBuilder builder = new StringBuilder(projectName);
		// Go through path
		try {
			SWTBotTreeItem item = tree.expandNode(projectName);
			for (String nodeName : path) {
				builder.append("/" + nodeName);
				item = item.expandNode(nodeName);				
			}
		} catch (WidgetNotFoundException e) {
			log.info("Node not found:" + builder.toString());
			return false;
		}

		return true;
	}
	
	private SWTBotTreeItem getItem(SWTBotTreeItem ancestor, String name) {
		try {
			return ancestor.expandNode(name);
		}
		catch (WidgetNotFoundException ex) {
			return null;
		}
	}
	
	

}
