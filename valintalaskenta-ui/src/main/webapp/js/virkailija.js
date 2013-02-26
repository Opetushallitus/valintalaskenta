$(document).ready(function(){
/* Virkailija.js begins */

	var dropMenu = {
		build:function(){
			dropMenu.setTriggers();
		},
		setTriggers:function(){
			
			$('body').on('mouseover', '.dropmenu', function(){
				$(this).addClass('hover');
			});
			
			$('body').on('mouseout', '.dropmenu', function(){
				$(this).removeClass('hover');
			});
		}
	}


	var treelist = {
		handlers : {
			checkboxClickBlock : false
		},
		build:function(){
		/*
			$('.itemtree input[type="checkbox"]').each(function(){
				if($(this).prop('checked') == true || $(this).attr('checked') == true)
				{
					$(this).attr('checked', 'checked');
				}
			});
*/
		
		
			treelist.setTriggers();

		},		
		setTriggers:function(){
			/* Redundant due to AngularJS
			$('body').on('click', '.treelist.collapsible .collapse', function(event){
				
				branch = $(this).closest('li');
				console.log(branch);
				if(branch.hasClass('collapsed'))
				{
					branch.removeClass('collapsed');
				}
				else
				{
					branch.addClass('collapsed');
				}
			});
			*/
			
			// Set checkbox states of child and ancestor items checkboxes
			$('body').on('click', '.treelist.multiselect input[type="checkbox"]', function(event){

				
			
				// Check if an instance of this function is running
				if (treelist.handlers.checkboxClickBlock == false)
				{
					// Prevent triggering another instance this function while working
					treelist.handlers.checkboxClickBlock = true;
					
					// Determine root of this tree
					this_tree = $(this).closest('.treelist');
					
					// Determine root item of branch
					branch = $(this).closest('li');
				
					/*** Handle Child Items ***/
				
					// Determine affected child items
					if(branch.hasClass('selectall'))
					{
						subtargets = this_tree.find('input[type="checkbox"]');
					}
					else
					{
						subtargets = branch.find('input[type="checkbox"]');
					}
					
					// Determine clicked checkbox value
					clicked_item = false;
					if($(this).prop('checked') == true || $(this).attr('checked') == true)
					{
						clicked_item = true;
					}
					
					// Set child items checkbox state same as this
					subtargets.each(function(){
					
						// Check this item's checkbox state
						item = false;
						if($(this).prop('checked') == true || $(this).attr('checked') == true)
						{
							item = true;
						}
						
						// If different, trigger click to change
						if(item != clicked_item)
						{
							$(this).trigger('click');
						}
						
					});
					
					/*** Handle Parent Items ***/
					
					// Determine potentially affected parents
					suptargets = branch.parentsUntil('.treelist', 'li');
					
					// Check and change parent items state
					suptargets.each(function(){
					
						// Counters for checked children
						var c0 = 0;
						var c1 = 0;
					
						
						// Check how many children are checked
						$(this).children('.branch').find('input[type="checkbox"]').each(function(){
						
							// Check this item's checkbox state and add to counter
							if($(this).prop('checked') == true || $(this).attr('checked') == true)
							{
								c1++;
							}
							else
							{
								c0++;
							}
						});
						
						// Determine state of checkbox
						if (c0 > 0 && c1 > 0){state = 2;}
						else if (c0 == 0){state = 1;}
						else{state = 0;}
						
						// Check this item's checkbox state
						item = 0;
						this_checkbox = $(this).children('input[type="checkbox"]');
						if(this_checkbox.prop('checked') == true || this_checkbox.attr('checked') == true)
						{
							item = 1;
						}
						
						// In binary check, indeterminate is false
						match = state;
						if (match == 2){match = 0;}
						
						// If different, trigger click to change
						if(match != item)
						{
							this_checkbox.trigger('click');
						}
						
						// Set indetereminate if that is the case
						if (state == 2){this_checkbox.prop('indeterminate',true);}
						else{this_checkbox.prop('indeterminate',false);}
						
					});
					
					/*** Handle Select All Checkbox ***/
					
					// Counters for checked children
					var c0 = 0;
					var c1 = 0;
					
					alltargets = this_tree.find('li:not(.selectall)');

					alltargets.children('input[type="checkbox"]').each(function(){
						// Check this item's checkbox state and add to counter
						if($(this).prop('checked') == true || $(this).attr('checked') == true)
						{
							c1++;
						}
						else
						{
							c0++;
						}
					});
					
					// Determine state of checkbox
					if (c0 > 0 && c1 > 0){state = 2;}
					else if (c0 == 0){state = 1;}
					else{state = 0;}
					
					// Check this selectall checkbox state
					item = 0;
					this_checkbox = this_tree.find('li.selectall').children('input[type="checkbox"]');
					if(this_checkbox.prop('checked') == true || this_checkbox.attr('checked') == true)
					{
						item = 1;
					}
					
					// In binary check, indeterminate is false
					match = state;
					if (match == 2){match = 0;}
					
					// If different, trigger click to change
					if(match != item)
					{
						this_checkbox.trigger('click');
					}
					
					// Set indetereminate if that is the case
					if (state == 2){this_checkbox.prop('indeterminate',true);}
					else{this_checkbox.prop('indeterminate',false);}

					// Allow triggering a new instance of this function
					treelist.handlers.checkboxClickBlock = false;
				}
			});
		}
	}
		
		
function epicfail(msg){
	params = [];
	title = '<h3>Error</h3>';
	console.log(document.location);
	url = document.location;
	href = url.protocol+'//'+url.host+url.pathname+'#/';
	link = '<p><a href="'+href+'" class="popover-close">Return to root</a></p>';
	params.content = title+msg+link;
	params.close = false;
	popover.add(params);
}

$('body').on('click', '.epicfail-test', function(){
	html = 'Epic fail'
	epicfail('Epic Fail');
});
		
		
/*

**** POPOVER ****

// Dialog window has been generated by JSP or AngularJS, the result is inside #overlay -element and should look something like this:

<div class="popover-wrapper" id="dialogid1" style="z-index:1000">';
	<span class="popover-close">&#8203;</span>
	<div class="popover">
		<span class="popover-close">&#8203;</span>
		<div class="popover-header">
			** Dialog title **
		</div>
		<div class="popover-content">
			** Dialog content **
		</div>
	</div>
</div>


// Show dialog window with specific id
<button class="button" data-op-show="dialogid1">Open dialog</button>

// Hide dialog window with specific id
<button class="button" data-op-close="dialogid1">Open dialog</button>


// Dialog window is dynamically generated

// Dynamically generate dialog window by callback with params

params = [];
params.id = 'dialogid2';
params.title = 'Dialog heading'; 
params.content = '<p>Dialog content using html</p><a href="#" class="button" data-op-remove="">No</a><a href="#" class="button" >Yes</a>'

popover.add(params);


// Remove dynamically generated 

*** Targeting needs work ***

*/
		
		
		
	var popover = {
		handlers : {
			openPopovers : 0,
			autoGenCount : 0
		},
		build:function(){
			popover.set.triggers();
		},
		add:function(params){
			// Popover auto-generated id
			id = 'popoverautogerenated'+popover.handlers.autoGenCount; 
			popover.handlers.autoGenCount++;
		
			if (typeof params.id != 'undefined')
			{
				if (params.id.length > 0)
				{
					id = params.id;
				}
			}
		
			angular_template = '';
			if (typeof params.src != 'undefined')
			{
				if (params.template.length > 0)
				{
					angular_template = 'ng-include="'+params.template+'"';
				}
			}
			
			popover_close = '<span class="popover-close">&#8203;</span>';
			
			/*
			// If type is alert, popover can only be closed by clicking certain link or button for triggering predefined callback
			if (params.type == 'alert')
			{
				if (params.close == false)
				{
					popover_close = '';
				}
			}
			*/
			
			title = '';
			if (typeof params.title != 'undefined')
			{
				if (params.title.length > 0)
				{
					title = params.title;
				}
			}
			
			content = '';
			if (typeof params.content != 'undefined')
			{
				if (params.content.length > 0)
				{
					content = params.content;
				}
			}
			
			content = 'test <a class="popovertest" data-po-add="new" href="#">Test</a>';
			title = 'Laatikon otsikko';
		
			html =  '<div class="popover-wrapper autogenerated" id="'+id+'" style="z-index:'+(popover.handlers.autoGenCount*100)+';">';
			html += 	popover_close;
			html += 	'<div class="popover">';
			html += 		popover_close;
			html += 		'<div class="popover-header">';
			html += 			title;
			html += 		'</div>';
			html += 		'<div class="popover-content">';
			html += 			content;
			html += 		'</div>';
			html += 	'</div>';
			html += '</div>';
		
			$('#overlay').append(html);
		
			popover.handlers.openPopovers++;
			popover.set.overlay();
			popover.set.size($('#'+id+' .popover'));
			popover.set.position($('#'+id+' .popover'));
		},
		hide:function(id){
			if($('#'+id).length != 0)
			{
				$('#'+id).hide();
				popover.handlers.openPopovers--;
				popover.set.overlay();
			}
		},
		remove:function(target){
			if(target.length != 0 && $(target).length != 0)
			{
				$(target).closest('.popover-wrapper').remove(); // Alternatively .detach()
				popover.handlers.openPopovers--;
				popover.set.overlay();
			}
		},
		show:function(id){
			if($('#'+id).length != 0)
			{
				$('#'+id).show();
				popover.handlers.openPopovers++;
				popover.set.overlay();
				popover.set.size($('#'+id+' .popover'));
				popover.set.position($('#'+id+' .popover'));
			}
		},
		set : {
			active:function(){
				$('#overlay .popover-wrapper').addClass('inactive').last().removeClass('inactive');
			},
			overlay:function(){
			
				// Show overlay if 1 or more popovers are open/visible
				// Hide overlay if no popovers are open/visible
				if(popover.handlers.openPopovers > 0)
				{
					$('#overlay').show();
					
					popover.set.active();
				}
				else
				{
					$('#overlay').hide();
				}
			},
			position:function(target){
			
				// Target the actual popover-window
				if($(target).hasClass('.popover-wrapper'))
				{
					target = $(target).find('.popover');
				}
			
				// Get window height and position from top
				window = $(window);
				window_top = $(window).scrollTop();
				window_height = $(window).height();
				
				// Get wrapper position from top
				wrapper_top = $('#viewport').scrollTop();
				popover_height = $(target).outerHeight(true);
				
				// Center popover if it fits in the window
				if (popover_height < window_height)
				{
					offset = (window_height-popover_height)/2;
				}
				else
				{
					offset = 0;
				}
				// Determine popover position
				popover_position = window_top+offset-wrapper_top;
				// console.log(window_top+"+"+offset+"-"+wrapper_top+"="+popover_position);
				target.css({'top':popover_position+'px'});
				
			},
			size:function(target){
				
				// Target the actual popover-window
				if($(target).hasClass('.popover-wrapper'))
				{
					target = $(target).find('.popover');
				}
				
				content_width = $(target).find('.popover-content').width();
				content_outerwidth = $(target).find('.popover-content').outerWidth(true);
				content_padding = content_outerwidth-content_width;

				// Content area has minimum width
				if (content_outerwidth < 460)
				{
					content_width = 460-content_padding;
				}
				
				popover_width = content_width-content_padding;
				
				console.log(content_width);
				
				$(target).find('.popover-content').css({'width':content_width+'px'});
				$(target).css({'width':popover_width+'px'});
				
			},
			triggers:function(){
			
				// Remove or hide popover from closing links
				$('body').on('click', '.popover-wrapper .popover-close', function(){
					
					// If window was generated dynamically remove, else just hide
					if($(this).closest('.popover-wrapper').hasClass('autogenerated'))
					{
						target = $(this).closest('.popover-wrapper').find('.popover');
						popover.remove(target);
					}
					else
					{
						id = $(this).closest('.popover-wrapper').attr('id');
						popover.hide(id);
					}
				});
				
				// Generate new popover
				$('body').on('click', '[data-po-add]', function(event){
					event.preventDefault();
					popover.add();

				});
				
				// Show already existing popover with id
				$('body').on('click', '[data-po-show]', function(event){
					event.preventDefault();
					id = $(this).attr('data-po-show');
					popover.show(id);
				});
				
				// Hide already existing popover with id
				$('body').on('click', '[data-po-hide]', function(event){
					event.preventDefault();
					id = $(this).attr('data-po-hide');
					popover.hide(id);
				});
			}
		}
	}
		
		/* Older version
var popover = {
		handlers : {
			count : 0
		},
		build:function(){
			popover.setTriggers();
		},
		generate:function(params){

			console.log(params);
			angular_template = '';
			if (typeof params.src != 'undefined')
			{
				if (params.template.length > 0)
				{
					angular_template = 'ng-include="'+params.template+'"';
				}
			}
			
			popover_close = '<span class="popover-close">&#8203;</span>';
			if (params.type == 'alert')
			{
				if (params.close == false)
				{
					popover_close = '';
				}
			}
			
			content = 'Lorem ipsum dolor sit amet. <span class="button small popover-test" data-popover="/jono/:id/new">Uusi popover</span>';
			id = 'popover_'+popover.handlers.count;
			popover.handlers.count++;
			html = 	'<div class="popover-wrapper" id="'+id+'" style="z-index:'+(popover.handlers.count*100)+';">'
						+popover_close
						+'<div class="popover">'
							+popover_close
							+'<div class="popover-content" '+angular_template+'>'
								+params.content
							+'</div>'
						+'</div>'
					+'</div>';
					
			$('#wrapper').prepend(html);
			
			this_popover = $('.popover-wrapper[id="'+id+'"]').find('.popover:first');
			
			
			// $compile(element);
			// angular.bootstrap(document, []);
			// Set popover width according to content
			popover_width = this_popover.find('.popover-content').outerWidth(true);
			this_popover.css({'width':popover_width+'px'});
			
			// Set popover position within window
			popover.setPosition($('.popover-wrapper[id="'+id+'"]').find('.popover:first'));
		},
		open:function(){
		
		},
		close:function(){
			
		},
		setPosition:function(target){
		
			// Get window height and position from top
			window_top = $(window).scrollTop();
			window_height = $(window).height();
			
			// Get wrapper position from top
			wrapper_top = $('#wrapper').scrollTop();
			console.log(wrapper_top);
			popover_height = $(target).outerHeight(true);
			
			// Center popover if it fits in the window
			if (popover_height < window_height)
			{
				offset = (window_height-popover_height)/2;
			}
			else
			{
				offset = 0;
			}
			// Determine popover position
			popover_position = window_top+offset-wrapper_top;
			console.log(window_top+' + ( '+window_height+' - '+popover_height+' ) = '+popover_position);
			$(target).css({'top':popover_position+'px'});
			
		},
		setTriggers:function(){
			$('body').on('click', '.popover-test', function(){
				params = [];
				params.template = $(this).attr('data-popover');
				console.log(params);
				popover.generate(params);
			});
		
			$('body').on('click', '.popover-wrapper .popover-close', function(){
				$(this).closest('.popover-wrapper').remove(); // Alternatively .detach()
			});
			
		}
	}
	*/
		
	dropMenu.build();
	popover.build();
	treelist.build();

});